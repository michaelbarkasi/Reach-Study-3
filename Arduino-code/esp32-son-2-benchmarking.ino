/*
 * 
 *  Sound Generation:
 *  
 * Setup base wave form @ 152,380 Hz using the ESP32's hardware PWM
 *     152,380 Hz is the fastest oscilation its hardware PWM can achieve with at least 9 bit of resolution
 *       Get this with: clk_src = LEDC_APB_CLK (80 MHz).
 *       ... then duty resolution is integer (log 2 (LEDC_APB_CLK / frequency))
 *     see: 
 *       https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/ledc.html
 *       https://github.com/espressif/esp-idf/blob/5893797bf068ba6f72105fff289ead370b4591a3/examples/peripherals/ledc/ledc_basic/main/ledc_basic_example_main.c
 * Initial code to get PWM working from: https://randomnerdtutorials.com/esp32-pwm-arduino-ide/
 * Note: We really want this PWM to run in LEDC_HIGH_SPEED_MODE; the code from the esp-idf gives this control, but not the code for arduino;
 *   Currently; assuming arduino puts PWM into high-speed mode by default
 *   
 *  BlueTooth: 
 *  
 * Based on the Arduino SerialToSerialBT example, which came with the following preface:
 * 
 * //This example code is in the Public Domain (or CC0 licensed, at your option.)
 * //By Evandro Copercini - 2018
 * //
 * //This example creates a bridge between Serial and Classical Bluetooth (SPP)
 * //and also demonstrate that SerialBT have the same functionalities of a normal Serial
 * 
*/

const int update_rate = 1000; // Controls both sound updates and when to process sensor data/ update motion models
const float dt = 1.0 / update_rate;

#define DEMO // uncomment to switch to just 5 random motions before sonification
//#define SENSOR3 // uncomment to use sensor 3 as well
//#define RUNHALFSPEED // uncomment to run sensors at 225 Hz
#define RUNFULLSPEED // uncomment to run sensors at 1100 Hz (gyro) and 1125 Hz (accel) 
#define SUBTRACTGYROBIAS // uncomment to subtract estimated gyro bias from raw gyro readings
//#define USEBT // uncomment to communicate over BlueTooth instead of wired serial port
//#define DACwave // uncomment to use DAC instead of ttPWM for sound generation

#ifdef SENSOR3 
  #include "ICM_20948.h" // Click here to get the library: http://librarymanager/All#SparkFun_ICM_20948_IMU
  // Definitions for Interrupts
  #define INT_PIN_myICM1 33//13 
  #define INT_PIN_myICM2 32//12
  #define INT_PIN_myICM3 14//27
  // Definitions for SPI
  #define SPI_PORT SPI     
  #define SPI_FREQ 4500000 // 7000000 is max for Sen20948 (see datasheet)? other TDK sheets suggest 2.5M
  #define CS_PIN_myICM1 27//33     
  #define CS_PIN_myICM2 21//15
  #define CS_PIN_myICM3 15//32  
  ICM_20948_SPI myICM1; // Arbitrary name for the sensor
  ICM_20948_SPI myICM2; // Arbitrary name for the sensor
  ICM_20948_SPI myICM3; // Arbitrary name for the sensor
  // Variables for interrupts (pulling sensor data)
  volatile bool isrFired1 = false;
  volatile bool isrFired2 = false;
  volatile bool isrFired3 = false;
#else 
  #include "ICM_20948.h" // Click here to get the library: http://librarymanager/All#SparkFun_ICM_20948_IMU
  // Definitions for Interrupts
  #define INT_PIN_myICM1 33//13 
  #define INT_PIN_myICM2 32//12
  // Definitions for SPI
  #define SPI_PORT SPI     
  #define SPI_FREQ 4500000 // 7000000 is max for Sen20948 (see datasheet)? other TDK sheets suggest 2.5M
  #define CS_PIN_myICM1 27//33     
  #define CS_PIN_myICM2 21//15
  ICM_20948_SPI myICM1; // Arbitrary name for the sensor
  ICM_20948_SPI myICM2; // Arbitrary name for the sensor
  // Variables for interrupts (pulling sensor data)
  volatile bool isrFired1 = false;
  volatile bool isrFired2 = false;
#endif

// Variables controlling base pulse wave production / volume control
#ifdef DACwave
  const int soundPin = A1;  // DAC is set to use DAC1, which is A1; DAC2 is A0
  const int amp_max_true = 255; // DAC is 8-bit, so only 256 levels
#else 
  const int soundPin = 25; // pin G (GPIO) 25 is actually the same pin on breakout board as A1
  const int base_wave_freq = 152380; // highest availabe with 9 bit resolution is 152380
  const int pwm_Channel = 0;
  const int pwm_resolution = (int) log2( 80000000 / base_wave_freq ) ;
  const int amp_max_true = pow(2,pwm_resolution) - 1; // for 9 bit resolution: 2^9 - 1 = 511; for 8 bit 2^8 - 1 = 255
#endif

#ifdef DEMO
  const int amp_max = (int) (amp_max_true*0.5);
#else
  const int amp_max = amp_max_true;
#endif

// Basic audio parameters
volatile int amp = (int) amp_max;
const int amp_default = (int) amp_max;
const int pitchinitial = 440;
volatile int TF = pitchinitial;
const int PitchValueMax = 6000;
const int PitchValueMin = 220;
float a_amp = 1.0;
float a_pitch = 1.0;

// setting the timer for generating hearable oscilation 
hw_timer_t * timerSW = NULL;
portMUX_TYPE timerSWMux = portMUX_INITIALIZER_UNLOCKED;
const int timerSWprescaler = 2; // Timers are 80Mhz; counters seem to be at least 32bit, so, don't need to prescale down low
const long timerSize = 80000000;
const int timerSizeprescaler = timerSize/timerSWprescaler;
volatile int timerSW_top = (int) timerSizeprescaler / (TF * 2); // controls frequency of hearable oscilation
volatile bool up = true;

// Variables for the timer controlling sound updates / motion processing
hw_timer_t * timerSU = NULL;
portMUX_TYPE timerSUMux = portMUX_INITIALIZER_UNLOCKED;
const int timerSU_top = (int) timerSizeprescaler / update_rate; 

// Setup for Bluetooth:
#ifdef USEBT 
  #include "BluetoothSerial.h"
  #if !defined(CONFIG_BT_ENABLED) || !defined(CONFIG_BLUEDROID_ENABLED)
  #error Bluetooth is not enabled! Please run `make menuconfig` to and enable it
  #endif
  BluetoothSerial SerialBT;
  #define SERIAL_PORT SerialBT 
#else
  #define SERIAL_PORT Serial
#endif

// Core Vector Variables
#ifdef SENSOR3 
  struct Vectors {
    float r = 0.0; // Uncomment if we need to use quaternions, in which case this is q0, hence, it should initialize as zero, in case it doesn't get set elsewhere and we're dealing with a vector in R^3
    float x = 0.0; // q1; if ever encoding quaternions, think of x/y/z as q1/q2/q3.  
    float y = 0.0; // q2; it doesn't matter what we initialize these values as, they all get overwritten. 0/1/0/0 is a holdover from previous programs that used quaternions.
    float z = 0.0; // q3
  } Sensor1_Acc, Sensor1_Gyro, // Sensor1_Mag, 
    Sensor2_Acc, Sensor2_Gyro, // Sensor2_Mag,
    Sensor3_Acc, Sensor3_Gyro, // Sensor3_Mag,
    Sensor1_Rot, Sensor2_Rot, Sensor3_Rot,
    Sensor1_Quat, Sensor2_Quat, Sensor3_Quat,
    Sensor1_GravityIP, Sensor2_GravityIP, Sensor3_GravityIP,
    Sensor1_Xaxis, Sensor1_Yaxis, Sensor1_Zaxis,
    Sensor2_Xaxis, Sensor2_Yaxis, Sensor2_Zaxis,
    Sensor3_Xaxis, Sensor3_Yaxis, Sensor3_Zaxis,
    Sensor1_XaxisIP, Sensor1_YaxisIP, Sensor1_ZaxisIP,
    Sensor2_XaxisIP, Sensor2_YaxisIP, Sensor2_ZaxisIP,
    Sensor3_XaxisIP, Sensor3_YaxisIP, Sensor3_ZaxisIP,
    Sensor1_GyroBias, Sensor2_GyroBias, Sensor3_GyroBias,
    zerov;
#else
  struct Vectors {
    float r = 0.0; // Uncomment if we need to use quaternions, in which case this is q0, hence, it should initialize as zero, in case it doesn't get set elsewhere and we're dealing with a vector in R^3
    float x = 0.0; // q1; if ever encoding quaternions, think of x/y/z as q1/q2/q3.  
    float y = 0.0; // q2; it doesn't matter what we initialize these values as, they all get overwritten. 0/1/0/0 is a holdover from previous programs that used quaternions.
    float z = 0.0; // q3
  } Sensor1_Acc, Sensor1_Gyro, // Sensor1_Mag, 
    Sensor2_Acc, Sensor2_Gyro, // Sensor2_Mag
    Sensor1_Rot, Sensor2_Rot, 
    Sensor1_Quat, Sensor2_Quat,
    Sensor1_GravityIP, Sensor2_GravityIP,
    Sensor1_Xaxis, Sensor1_Yaxis, Sensor1_Zaxis,
    Sensor2_Xaxis, Sensor2_Yaxis, Sensor2_Zaxis,
    Sensor1_XaxisIP, Sensor1_YaxisIP, Sensor1_ZaxisIP,
    Sensor2_XaxisIP, Sensor2_YaxisIP, Sensor2_ZaxisIP,
    Sensor1_GyroBias, Sensor2_GyroBias,
    zerov;
#endif

// Motion Processing Variables
const float Gyro_multiple = PI / (16.4 * 180.0); // puts gyro readings in radians / s
const float Acc_multiple = 9.80665 / 2048.0; // puts accel readings in m/s^2
int reachnum = 1; 
int randomsamplenum = 0; // which of the first random motions is to be treated as the model during sonification? (Set randomly)
#ifdef DEMO
 int randomsamplesize = 5; // How many random samples should be collected before sonifying? 
#else
 int randomsamplesize = 25; // How many random samples should be collected before sonifying? 
#endif
bool IPfound = false;
const float IPtolerancefraction = 0.1; // This fraction used to set the above value, e.g. ".1 = within 10 percent". 
float IPtolerance = 1.0; // How close to the gravity vectors recorded at the start do we need to be for the unit to think user is in the IP? 
unsigned long time1 = 0; // time1, time2, and time3 used to time how long we've been recording, during live sonification
unsigned long time2 = 0;
unsigned long time3 = 0;
float samplerate = 1.0;
int Realsamplecount = 0; // because of the compensation for different velocity, this comes apart from the MOTsamplecount
int MOTsamplecount = 0;
int MOTsamplecount_max = 0;
int MOTadvance = 1; 
float to_end_start = 1.0;
float error1 = 0;
float error2 = 0;
#ifdef SENSOR3
  float error3 = 0;
#endif
float error_total = 0;
const int MaxReadingsToSave = 1400;
long time_pwm_handling = 0;
long time_fetch_sensor1 = 0;
long time_motion_check = 0;
long time_motion_processing = 0;
long time_error_computing = 0;
long time_sonification_computing = 0;
long time_pitch_update = 0;
long time_MOT_warping = 0;
long time_pwm_handlingS[MaxReadingsToSave] = {0};
long time_fetch_sensor1S[MaxReadingsToSave] = {0};
long time_motion_checkS[MaxReadingsToSave] = {0};
long time_motion_processingS[MaxReadingsToSave] = {0};
long time_error_computingS[MaxReadingsToSave] = {0};
long time_sonification_computingS[MaxReadingsToSave] = {0};
long time_pitch_updateS[MaxReadingsToSave] = {0};
long time_MOT_warpingS[MaxReadingsToSave] = {0};
struct Vectors model_q1[MaxReadingsToSave];
struct Vectors model_q2[MaxReadingsToSave];
int MOTsamplecount_saved[MaxReadingsToSave] = {0};
#ifdef SENSOR3
  struct Vectors quat3_saved[MaxReadingsToSave];
  struct Vectors model_q3[MaxReadingsToSave];
#endif
bool FIRSTSAMPLE = true;
bool IN_MOTION = false;
bool WAITING_FOR_REST = true;
bool WAITING_FOR_INITATION = true;
bool MOTION_JUST_ENDED = false;
bool MOTION_JUST_STARTED = false;
int jostle_buff_count = 0;
int jostle_buff_count_saved = 1;
int jostle_noise_filter = 100;
int rest_buff_count = 0;
int rest_noise_filter = 100; 
#define MOTIONCHECKSENSOR_Gyro Sensor2_Gyro // What sensor are we using for the motion check?
const float motionthreshold_gyro = (12.0 * (PI/180.0)) * (12.0 * (PI/180.0)); // How fast (in radians per second) must a sensor be moving (i.e., rotating) for it to be considered "in motion"?
                              // Number here is magnetude of the *total* rotation <x,y,z> vector, not rotation about any one axis. (3 degrees per axis is standard noise)
const float motionthreshold_gyro_high = (motionthreshold_gyro * 3.0) * (motionthreshold_gyro * 3.0);

// Interrupt functions: 
bool UPDATE_SOUND_NOW = true; // The timer (TC2) controlling sound update rate will flip this on when it's time to read.
void IRAM_ATTR onTimerSU() { // interrupt handler for timer generating sound updates / motion processing: 
  portENTER_CRITICAL_ISR(&timerSWMux);
  UPDATE_SOUND_NOW = true;
  portEXIT_CRITICAL_ISR(&timerSWMux);
}
void IRAM_ATTR onTimerSW() { // for timer generating hearable oscilation
  portENTER_CRITICAL_ISR(&timerSWMux);
  time_pwm_handling = micros();
  #ifdef DACwave
    if (up) {
      dacWrite(DAC1, amp);
    } else {
      dacWrite(DAC1, 0);
    }
  #else 
    if (up) {
      ledcWrite(pwm_Channel, amp);
    } else {
      ledcWrite(pwm_Channel, 0);
    }
  #endif
  up = !up;
  time_pwm_handling = micros() - time_pwm_handling;
  portEXIT_CRITICAL_ISR(&timerSWMux);
}
void icmISR1(void) { // These three for grabbing sensor data
  isrFired1 = true; // Can't use I2C within ISR on 328p, so just set a flag to know that data is available
}
void icmISR2(void) {
  isrFired2 = true; // Can't use I2C within ISR on 328p, so just set a flag to know that data is available
}
#ifdef SENSOR3
  void icmISR3(void) {
    isrFired3 = true; // Can't use I2C within ISR on 328p, so just set a flag to know that data is available
  }
#endif

void setup() {

  // Setup serial port / BlueTooth
  Serial.begin(115200);
  #ifdef USEBT
    SerialBT.begin("Wave APS (ESP32) Sonification Unit"); // BlueTooth device name
    Serial.println("The device started, now you can pair it with bluetooth!");
  #else 
    Serial.println("The device started!");
  #endif

  // Pause all setup until connected to serial / BlueTooth
  while (!SERIAL_PORT.available()) {
    SERIAL_PORT.println(F("Press any key to begin."));
    delay(6000);
  }
  #ifdef USEBT 
    SERIAL_PORT.println(F("Connected to serial port through Bluetooth!"));
  #else 
    SERIAL_PORT.println(F("Connected to serial port!"));
  #endif

  SERIAL_PORT.println();
  SERIAL_PORT.println(F("Program: Real Sonification"));

  SERIAL_PORT.println();
  SERIAL_PORT.print(F("Update rate selected: "));
  SERIAL_PORT.println(update_rate,1);

  SERIAL_PORT.println();
  SERIAL_PORT.print(F("Setting up base pulse wave: "));
  
  // Setup base pulse wave
  #ifdef DACwave
    SERIAL_PORT.println(F("DAC selected"));
    SERIAL_PORT.print(F("Volume resolution: "));
    SERIAL_PORT.println(amp_max,1);
  #else
    SERIAL_PORT.println(F("ttPWM selected"));
    SERIAL_PORT.print(F("Volume resolution: "));
    SERIAL_PORT.println(amp_max);
    SERIAL_PORT.print(F("Base wave frequency: "));
    SERIAL_PORT.println(base_wave_freq,1);
    ledcSetup(pwm_Channel, base_wave_freq, pwm_resolution); // configure PWM functionalitites
    ledcAttachPin(soundPin, pwm_Channel); // attach the channel to the pin generating the wave
  #endif
  SERIAL_PORT.println();
  
  // Setup timer used for hearable oscilation
  timerSW = timerBegin(0, timerSWprescaler, true); // true indicates counter goes up
  timerAttachInterrupt(timerSW, &onTimerSW, true); // true indicates edge interrupt (false for level)
  timerAlarmWrite(timerSW, timerSW_top, true); // true for reload automatically
  timerAlarmEnable(timerSW);
  
  // Setup timer for sound updates/ motion processing
  timerSU = timerBegin(1, timerSWprescaler, true); // true indicates counter goes up
  timerAttachInterrupt(timerSU, &onTimerSU, true); // true indicates edge interrupt (false for level)
  timerAlarmWrite(timerSU, timerSU_top, true); // true for reload automatically
  timerAlarmEnable(timerSU);

  // Enable SPI communication 
  SPI_PORT.begin();

  #ifdef SENSOR3
  
    myICM1.enableDebugging(); // Uncomment this line to enable helpful debug messages on Serial about ICM1
    myICM2.enableDebugging(); // Uncomment this line to enable helpful debug messages on Serial about ICM2
    myICM3.enableDebugging(); // Uncomment this line to enable helpful debug messages on Serial about ICM3
  
    // Setup Interrupts
    // myICM1
    pinMode(INT_PIN_myICM1, INPUT_PULLUP); // Using a pullup b/c ICM-20948 Breakout board has an onboard pullup as well and we don't want them to compete
    attachInterrupt(digitalPinToInterrupt(INT_PIN_myICM1), icmISR1, FALLING); // Set up a falling interrupt; 
    // myICM2
    pinMode(INT_PIN_myICM2, INPUT_PULLUP);                                   
    attachInterrupt(digitalPinToInterrupt(INT_PIN_myICM2), icmISR2, FALLING); 
    // myICM3
    pinMode(INT_PIN_myICM3, INPUT_PULLUP);                                   
    attachInterrupt(digitalPinToInterrupt(INT_PIN_myICM3), icmISR3, FALLING); 
  
    // Initialize the sensors over SPI
    initializeSensor_SPI (myICM1, CS_PIN_myICM1, 1);
    initializeSensor_SPI (myICM2, CS_PIN_myICM2, 2);
    initializeSensor_SPI (myICM3, CS_PIN_myICM3, 3);
  
    // Configure the DMP for each sensor
    // configureDMP (myICM1);
    // configureDMP (myICM2);
    // configureDMP (myICM3);
  
    // Configure sensors for raw data pulls
    configureSensor (myICM1);
    configureSensor (myICM2);
    configureSensor (myICM3);
  
    // Configure interrupts for each sensor
    configureInterrupts (myICM1);
    configureInterrupts (myICM2);
    configureInterrupts (myICM3);

  # else 
  
    myICM1.enableDebugging(); // Uncomment this line to enable helpful debug messages on Serial about ICM1
    myICM2.enableDebugging(); // Uncomment this line to enable helpful debug messages on Serial about ICM2
  
    // Setup Interrupts
    // myICM1
    pinMode(INT_PIN_myICM1, INPUT_PULLUP); // Using a pullup b/c ICM-20948 Breakout board has an onboard pullup as well and we don't want them to compete
    attachInterrupt(digitalPinToInterrupt(INT_PIN_myICM1), icmISR1, FALLING); // Set up a falling interrupt; 
    // myICM2
    pinMode(INT_PIN_myICM2, INPUT_PULLUP);                                   
    attachInterrupt(digitalPinToInterrupt(INT_PIN_myICM2), icmISR2, FALLING); 
  
    // Initialize the sensors over SPI
    initializeSensor_SPI (myICM1, CS_PIN_myICM1, 1);
    initializeSensor_SPI (myICM2, CS_PIN_myICM2, 2);
  
    // Configure the DMP for each sensor
    // configureDMP (myICM1);
    // configureDMP (myICM2);
  
    // Configure sensors for raw data pulls
    configureSensor (myICM1);
    configureSensor (myICM2);
  
    // Configure interrupts for each sensor
    configureInterrupts (myICM1);
    configureInterrupts (myICM2);

  #endif

  // Set axes: 
  Sensor1_Xaxis.x = 1.0;
  Sensor1_Yaxis.y = 1.0;
  Sensor1_Zaxis.z = 1.0;
  Sensor2_Xaxis.x = 1.0;
  Sensor2_Yaxis.y = 1.0;
  Sensor2_Zaxis.z = 1.0;
  #ifdef SENSOR3
    Sensor3_Xaxis.x = 1.0;
    Sensor3_Yaxis.y = 1.0;
    Sensor3_Zaxis.z = 1.0;
  #endif

  Sensor1_XaxisIP = Sensor1_Xaxis;
  Sensor1_YaxisIP = Sensor1_Yaxis;
  Sensor1_ZaxisIP = Sensor1_Zaxis;
  Sensor2_XaxisIP = Sensor2_Xaxis;
  Sensor2_YaxisIP = Sensor2_Yaxis;
  Sensor2_ZaxisIP = Sensor2_Zaxis;
  #ifdef SENSOR3
    Sensor3_XaxisIP = Sensor3_Xaxis;
    Sensor3_YaxisIP = Sensor3_Yaxis;
    Sensor3_ZaxisIP = Sensor3_Zaxis;
  #endif

  SERIAL_PORT.println();
  SERIAL_PORT.println(F("Configuration complete!"));

  int IPcounter = 0;
  int IPcounterMax = 2000;
  bool IPSAVED = false;

  amplitude_update(0);

  SERIAL_PORT.println();
  SERIAL_PORT.println(F("Begin OPTITRACK reading in 5 seconds ... "));

  for ( int i = 5 ; i >= 0 ; i-- ) {

    if ( i > 0 ) {
      SERIAL_PORT.println();
      SERIAL_PORT.println(i,1);
      delay(1000);
    } else {
      SERIAL_PORT.println();
      SERIAL_PORT.println(F("NOW!"));
    }
    
  }

  delaywithFIFOreset(500);

  // Pause until user is still
  SERIAL_PORT.println();
  SERIAL_PORT.println(F("Must Capture IP and gyro drift; press any key when still."));
  while (SERIAL_PORT.available()) {
    SERIAL_PORT.read();
    delay(1);
  }
  while (!SERIAL_PORT.available()) {
    delay(1);
  }

  delaywithFIFOreset(50);

  while (!IPSAVED) { // Grab IP and estimate gyro drift

    if (isrFired1) { // If our isr flag is set then clear the interrupts on the ICM
      isrFired1 = false;
      FetchReadingsSensor (myICM1, Sensor1_Acc, Sensor1_Gyro); //, Sensor1_Mag); // Not pulling Mag data
    }
    if (isrFired2) { // If our isr flag is set then clear the interrupts on the ICM
      isrFired2 = false;
      FetchReadingsSensor (myICM2, Sensor2_Acc, Sensor2_Gyro); //, Sensor2_Mag); 
    }
    #ifdef SENSOR3
      if (isrFired3) { // If our isr flag is set then clear the interrupts on the ICM
        isrFired3 = false;
        FetchReadingsSensor (myICM3, Sensor3_Acc, Sensor3_Gyro); //, Sensor3_Mag);
      }
    #endif

    if (IPcounter == 0) {
      Sensor1_GravityIP = Sensor1_Acc; // Grab the acceleration vector as the gravity vector at IP
      Sensor2_GravityIP = Sensor2_Acc;
      #ifdef SENSOR3
        Sensor3_GravityIP = Sensor3_Acc;
      #endif
      Sensor1_GyroBias = Sensor1_Gyro; // the gyro should be at rest (0,0,0), so whatever readings we get here need to be subtracted as we integrate
      Sensor2_GyroBias = Sensor2_Gyro;
      #ifdef SENSOR3
        Sensor3_GyroBias = Sensor3_Gyro;
      #endif
      SERIAL_PORT.println();
      SERIAL_PORT.println(F("grabbing IP."));
    }
    if (IPcounter > 0) {
      Sensor1_GravityIP = vrunning_avg(Sensor1_GravityIP,Sensor1_Acc,IPcounter); // Now take the vectors a bunch more and average them
      Sensor2_GravityIP = vrunning_avg(Sensor2_GravityIP,Sensor2_Acc,IPcounter);
      #ifdef SENSOR3
        Sensor3_GravityIP = vrunning_avg(Sensor3_GravityIP,Sensor3_Acc,IPcounter);
      #endif
      Sensor1_GyroBias = vrunning_avg(Sensor1_GyroBias,Sensor1_Gyro,IPcounter);
      Sensor2_GyroBias = vrunning_avg(Sensor2_GyroBias,Sensor2_Gyro,IPcounter);
      #ifdef SENSOR3
        Sensor3_GyroBias = vrunning_avg(Sensor3_GyroBias,Sensor3_Gyro,IPcounter);
      #endif
    }
    if (IPcounter == IPcounterMax) {
      IPSAVED = true;
      IPtolerance = IPtolerancefraction * vmag(Sensor1_GravityIP);           
      SERIAL_PORT.println();
      SERIAL_PORT.println(F("IP Saved! Gravity Vectors:"));
      print_vector3ln(Sensor1_GravityIP);
      print_vector3ln(Sensor2_GravityIP);
      #ifdef SENSOR3
        print_vector3ln(Sensor3_GravityIP);
      #endif
      float GMag1 = vmag (Sensor1_GravityIP);
      float GMag2 = vmag (Sensor2_GravityIP);
      #ifdef SENSOR3
        float GMag3 = vmag (Sensor3_GravityIP);
      #endif
      SERIAL_PORT.println();
      SERIAL_PORT.println(F("Gravity Vector Magnitudes:"));
      SERIAL_PORT.println(GMag1,3);
      SERIAL_PORT.println(GMag2,3);
      #ifdef SENSOR3
        SERIAL_PORT.println(GMag3,3);
      #endif
      Sensor1_GyroBias = vscaler_mult(Sensor1_GyroBias,dt);
      Sensor2_GyroBias = vscaler_mult(Sensor2_GyroBias,dt);
      #ifdef SENSOR3
        Sensor3_GyroBias = vscaler_mult(Sensor3_GyroBias,dt);
      #endif
      SERIAL_PORT.println();
      SERIAL_PORT.println(F("Gyro Bias Vectors: (How many degrees will the ROT drift per sample at rest?)"));
      print_vector3ln(Sensor1_GyroBias);
      print_vector3ln(Sensor2_GyroBias);
      #ifdef SENSOR3
        print_vector3ln(Sensor3_GyroBias);
      #endif 
      //Sensor1_GyroBias = vscaler_mult(Sensor1_GyroBias,dt);
      //Sensor2_GyroBias = vscaler_mult(Sensor2_GyroBias,dt);
      //Sensor3_GyroBias = vscaler_mult(Sensor3_GyroBias,dt);
      //SERIAL_PORT.println();
      //SERIAL_PORT.println(F("Gyro Bias / ROT drift Vectors: (How many degrees will the ROT drift per sample at rest?)"));
      //print_vector3(Sensor1_GyroBias);
      //print_vector3(Sensor2_GyroBias);
      //#ifdef SENSOR3
      //  print_vector3ln(Sensor3_GyroBias);
      //#endif 
    }
    
    IPcounter++;

    myICM1.clearInterrupts();
    myICM2.clearInterrupts();
    #ifdef SENSOR3
      myICM3.clearInterrupts();
    #endif

  }

  randomSeed(millis());
  randomsamplenum = random(1,(randomsamplesize -4)); 

  SERIAL_PORT.print(F("Random Sample Num (Model): "));
  SERIAL_PORT.println(randomsamplenum,1);
 
}

void loop() {

  // Always fetch readings as soon as interrupts are noticed
  if (isrFired1) { // If our isr flag is set then clear the interrupts on the ICM
    isrFired1 = false;
    time_fetch_sensor1 = micros();
    FetchReadingsSensor (myICM1, Sensor1_Acc, Sensor1_Gyro); //, Sensor1_Mag); // Not pulling Mag data
    time_fetch_sensor1 = micros() - time_fetch_sensor1;
  }
  if (isrFired2) { // If our isr flag is set then clear the interrupts on the ICM
    isrFired2 = false;
    FetchReadingsSensor (myICM2, Sensor2_Acc, Sensor2_Gyro); //, Sensor2_Mag); 
  }
  #ifdef SENSOR3
    if (isrFired3) { // If our isr flag is set then clear the interrupts on the ICM
      isrFired3 = false;
      FetchReadingsSensor (myICM3, Sensor3_Acc, Sensor3_Gyro); //, Sensor3_Mag);
    }
  #endif

  //  Run Motion Check (always keep tabs on if we're in motion or still) 
  float vmag_gyro = vmagSqrd(MOTIONCHECKSENSOR_Gyro);
  time_motion_check = micros();
  if (vmag_gyro < motionthreshold_gyro) { //make sure to select a sensor whose movement correlates well with the whole system
    if (jostle_buff_count_saved == jostle_buff_count) { // don't wait around if jostling isn't being detected
      rest_buff_count += 6;
    } else {
      rest_buff_count++;
    }
    if (rest_buff_count > rest_noise_filter) { // if not moving 
      IN_MOTION = false; // Signal we're at rest     
      if (WAITING_FOR_REST) MOTION_JUST_ENDED = true; // signal that motion has just ended
    }
    jostle_buff_count_saved = jostle_buff_count;
  } else { // else the gyro readings indicate movement
    if (vmag_gyro > motionthreshold_gyro_high) { // don't wait around if we're definitely moving
      jostle_buff_count += 11; 
    } else {
      jostle_buff_count++;
    }
    if (jostle_buff_count > jostle_noise_filter) { // If we're moving
      IN_MOTION = true; // Signal we're in motion
      if (WAITING_FOR_INITATION) MOTION_JUST_STARTED = true; // signal that we have initiated movement
    }
  }
  time_motion_check = micros() - time_motion_check;

  // There are certain routines that should run only once, as soon as the unit decides it's moving
  if (MOTION_JUST_STARTED) {
    Realsamplecount = 0; // Reset the two variables keeping track of the number of samples since motion has initiated
    MOTsamplecount = 0;
    MOTadvance = 1;
    rest_buff_count = 0; // Reset the rest buffer
    jostle_buff_count = 0; // Reset the jostle buffer
    WAITING_FOR_INITATION = false; // By flipping to false, unit is remembering (for next time through loop) that the motion hasn't just started
    MOTION_JUST_STARTED = false; // This status should turn itself off once it's run
    WAITING_FOR_REST = true;
    //PRINTED_RUN_SUMMARY_DATA = false; 
  }

  // There are certain routines that should run only once, as soon as the unit decides its still
  if (MOTION_JUST_ENDED) {
    time2 = micros(); 
    amplitude_update(0); // End to motion should always turn the sound off
    jostle_buff_count = 0; // Reset the jostle buffer
    rest_buff_count = 0; // Reset the rest buffer
    WAITING_FOR_REST = false; // By flipping to false, unit is remembering (for next time through loop) that the motion hasn't just ended
    MOTION_JUST_ENDED = false; // This status should turn itself off once it's run 
    WAITING_FOR_INITATION = true;

    if ( IPfound && Realsamplecount > 250 ) { // don't count short accidental jerks under 250ms

      if ( reachnum > randomsamplesize ) {
        delaywithFIFOreset(500);
        Playback_model();
      }
       
      time3 = time2 - time1;
      float samplingtime = (float)( (float)time3 / (float)1000000 );
  
      samplerate = Realsamplecount / samplingtime; 

      for ( int i = 0; i < MaxReadingsToSave; i++ ) {
        if ( i <= (Realsamplecount - 1) ) {
    
          if ( i < (Realsamplecount - 1) ) {
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(reachnum,1);
            SERIAL_PORT.print(F(", ")); 
            SERIAL_PORT.print(i,1); // print real sample count
            SERIAL_PORT.print(F(", ")); 
            SERIAL_PORT.print(amp,1);
            SERIAL_PORT.print(F(", ")); 
            SERIAL_PORT.print(TF,1);
            SERIAL_PORT.print(F(", ")); 
            SERIAL_PORT.print(error_total,5);
            SERIAL_PORT.print(F(", ")); 
            SERIAL_PORT.print(MOTsamplecount_saved[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(time_pwm_handlingS[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(time_fetch_sensor1S[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(time_motion_checkS[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(time_motion_processingS[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(time_error_computingS[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(time_sonification_computingS[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(time_pitch_updateS[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.println(time_MOT_warpingS[i],1);
          }
          if ( i == (Realsamplecount -1) ) {
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(reachnum,1);
            SERIAL_PORT.print(F(", ")); 
            SERIAL_PORT.print(i,1); // print real sample count
            SERIAL_PORT.print(F(", ")); 
            SERIAL_PORT.print(amp,1);
            SERIAL_PORT.print(F(", ")); 
            SERIAL_PORT.print(TF,1);
            SERIAL_PORT.print(F(", ")); 
            SERIAL_PORT.print(error_total,5);
            SERIAL_PORT.print(F(", ")); 
            SERIAL_PORT.print(MOTsamplecount_saved[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(time_pwm_handlingS[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(time_fetch_sensor1S[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(time_motion_checkS[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(time_motion_processingS[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(time_error_computingS[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(time_sonification_computingS[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(time_pitch_updateS[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(time_MOT_warpingS[i],1);
            SERIAL_PORT.print(F(", "));
            SERIAL_PORT.print(F("Samples: , "));
            SERIAL_PORT.print(Realsamplecount); 
            SERIAL_PORT.print(F(", Time: , "));
            SERIAL_PORT.print(samplingtime,3); 
            SERIAL_PORT.print(F(", Rate: , "));
            SERIAL_PORT.println(samplerate,1);
          }
        }
      }

      if ( reachnum == randomsamplenum ) {

        MOTsamplecount_max = Realsamplecount - 1; // Gets taken one to far, but okay, since starts at zero. 
        
        #ifdef SENSOR3
        to_end_start = vdst4(model_q1[0],model_q1[MOTsamplecount_max]) + 
                       vdst4(model_q2[0],model_q2[MOTsamplecount_max]) + 
                       vdst4(model_q3[0],model_q3[MOTsamplecount_max]);
        #else
        to_end_start = vdst4(model_q1[0],model_q1[MOTsamplecount_max]) + 
                       vdst4(model_q2[0],model_q2[MOTsamplecount_max]);
        #endif

        a_amp = ( amp_max - 0 ) / ( ( -1 * to_end_start ) * ( -1 * to_end_start ) );
        a_pitch = ( PitchValueMax - pitchinitial ) / ( ( -1 * to_end_start ) * ( -1 * to_end_start ) );
        
      }

      reachnum++;
      
    }

    IPfound = false; // Signal we're now out of the IP

    Sensor1_Rot = zerov;
    Sensor2_Rot = zerov;
    Sensor1_Quat = zerov;
    Sensor2_Quat = zerov;
    Sensor1_Xaxis = Sensor1_XaxisIP;
    Sensor1_Yaxis = Sensor1_YaxisIP;
    Sensor1_Zaxis = Sensor1_ZaxisIP;
    Sensor2_Xaxis = Sensor2_XaxisIP;
    Sensor2_Yaxis = Sensor2_YaxisIP;
    Sensor2_Zaxis = Sensor2_ZaxisIP;
    #ifdef SENSOR3
      Sensor3_Rot = zerov;
      Sensor3_Quat = zerov;
      Sensor3_Xaxis = Sensor3_XaxisIP;
      Sensor3_Yaxis = Sensor3_YaxisIP;
      Sensor3_Zaxis = Sensor3_ZaxisIP;
    #endif

    delaywithFIFOreset(1); 
    
  }

  if (!IN_MOTION && !IPfound) {
        
    if ( vdst(Sensor1_GravityIP,Sensor1_Acc) < IPtolerance ){ // We are near the IP
      if ( vdst(Sensor2_GravityIP,Sensor2_Acc) < IPtolerance ) {
        #ifdef SENSOR3
          if ( vdst(Sensor3_GravityIP,Sensor3_Acc) < IPtolerance ) {
        #endif

          IPfound = true;
          UpdateTF(pitchinitial);
          amplitude_update(amp_default);
          rest_buff_count = 0; // Reset the rest buffer so the unit response doesn't bounce around too much
          jostle_buff_count = 0; // Reset the jostle buffer so the unit response doesn't bounce around too much
          
        #ifdef SENSOR3
          }
        #endif
      }
    }

  }

  if ( UPDATE_SOUND_NOW && IN_MOTION && IPfound ) {

    if ( Realsamplecount == 0 ) {
      time1 = micros();
      FIRSTSAMPLE = true;
    }

    time_motion_processing = micros();
    compute_motion();

    time_motion_processing = micros() - time_motion_processing;

    if ( reachnum == randomsamplenum ) {

        MOTsamplecount_max = Realsamplecount - 1; // Gets taken one to far, but okay, since starts at zero. 
        
        if ( Realsamplecount < MaxReadingsToSave ) {
            model_q1[Realsamplecount] = Sensor1_Quat;
            model_q2[Realsamplecount] = Sensor2_Quat; 
            #ifdef SENSOR3
              model_q3[Realsamplecount] = Sensor3_Quat;
            #endif
        }
 
      }

    if ( reachnum > randomsamplesize ) {

      // compute error
      time_error_computing = micros();
      error1 = vdst4(Sensor1_Quat,model_q1[MOTsamplecount]); // max error = 2
      error2 = vdst4(Sensor2_Quat,model_q2[MOTsamplecount]); // max error = 2
      #ifdef SENSOR3
        error3 = vdst4(Sensor3_Quat,model_q3[MOTsamplecount]); // max error = 2
        error_total = error1 + error2 + erro3; // max of 6
        float to_end = vdst4(Sensor1_Quat,model_q1[MOTsamplecount_max]) + vdst4(Sensor2_Quat,model_q2[MOTsamplecount_max]) + vdst4(Sensor3_Quat,model_q3[MOTsamplecount_max]);
      #else 
        error_total = error1 + error2; // max of 4
        float to_end = vdst4(Sensor1_Quat,model_q1[MOTsamplecount_max]) + vdst4(Sensor2_Quat,model_q2[MOTsamplecount_max]);
      #endif
      time_error_computing = micros() - time_error_computing;

      // compute and update sound 
      time_sonification_computing = micros();
      amplitude_update( (int) ( ( a_amp * ( ( error_total - to_end_start ) * ( error_total - to_end_start ) ) ) + 0 ) );
      TF = (int) ( ( a_pitch * ( ( to_end - to_end_start ) * ( to_end - to_end_start ) ) ) + pitchinitial ); 
      if ( to_end > to_end_start ) {
        TF = TF * -1;
      } 
      time_sonification_computing = micros() - time_sonification_computing;
      time_pitch_update = micros();
      UpdateTF(TF);
      time_pitch_update = micros() - time_pitch_update;

      // save relevant data for printing at end of motion 
      if ( Realsamplecount < MaxReadingsToSave ) {
        MOTsamplecount_saved[Realsamplecount] = MOTsamplecount;
      }
      
      // determine what MOTsample we should be at, based on current QUAT vectors
      time_MOT_warping = micros();
      if ( MOTsamplecount < 100 ) { // first 100 MOTsamplecount are a special case; just advance once (initiation too noisey)
        MOTsamplecount++;
      }
      else {
        #ifdef SENSOR3
          float distn1 = vdstSq4(Sensor1_Quat,model_q1[MOTsamplecount-1]) +
                         vdstSq4(Sensor2_Quat,model_q2[MOTsamplecount-1]) + 
                         vdstSq4(Sensor3_Quat,model_q3[MOTsamplecount-1]);
          float dist0 = vdstSq4(Sensor1_Quat,model_q1[MOTsamplecount]) +
                        vdstSq4(Sensor2_Quat,model_q2[MOTsamplecount]) + 
                        vdstSq4(Sensor3_Quat,model_q3[MOTsamplecount]);
          float dist1 = vdstSq4(Sensor1_Quat,model_q1[MOTsamplecount+1]) +
                        vdstSq4(Sensor2_Quat,model_q2[MOTsamplecount+1]) + 
                        vdstSq4(Sensor3_Quat,model_q3[MOTsamplecount+1]);
        #else
          float distn1 = vdstSq4(Sensor1_Quat,model_q1[MOTsamplecount-1]) +
                         vdstSq4(Sensor2_Quat,model_q2[MOTsamplecount-1]);
          float dist0 = vdstSq4(Sensor1_Quat,model_q1[MOTsamplecount]) +
                        vdstSq4(Sensor2_Quat,model_q2[MOTsamplecount]);
          float dist1 = vdstSq4(Sensor1_Quat,model_q1[MOTsamplecount+1]) +
                        vdstSq4(Sensor2_Quat,model_q2[MOTsamplecount+1]);
        #endif 
        if ( MOTadvance == 0 ) {
          if ( dist1 < dist0 ) {
            MOTadvance = 2;
          } else {
            MOTadvance = 1;
          }
        } else {
          if ( distn1 < dist0 ) {
            MOTadvance = 0;
          } else if ( dist1 < dist0 ) {
            MOTadvance = 2;
          } else {
            MOTadvance = 1;
          }
        }
        MOTsamplecount = MOTsamplecount + MOTadvance; 
      }
      if (MOTsamplecount > MOTsamplecount_max) MOTsamplecount = MOTsamplecount_max; // don't let us go beyond MOTsamplecount_saved (returns gibberish for error next update)
      time_MOT_warping = micros() - time_MOT_warping;
    }

    if ( Realsamplecount < MaxReadingsToSave ) {
      time_pwm_handlingS[Realsamplecount] = time_pwm_handling;
      time_fetch_sensor1S[Realsamplecount] = time_fetch_sensor1;
      time_motion_checkS[Realsamplecount] = time_motion_check;
      time_motion_processingS[Realsamplecount] = time_motion_processing;
      time_error_computingS[Realsamplecount] = time_error_computing;
      time_sonification_computingS[Realsamplecount] = time_sonification_computing;
      time_pitch_updateS[Realsamplecount] = time_pitch_update;
      time_MOT_warpingS[Realsamplecount] = time_MOT_warping;
    }
    
    Realsamplecount++;
        
    UPDATE_SOUND_NOW = false;
    
  }

  myICM1.clearInterrupts();
  myICM2.clearInterrupts();
  #ifdef SENSOR3
    myICM3.clearInterrupts();
  #endif

}

void Playback_model( void ) {

  for ( int i = 0; i <= MOTsamplecount_max; i++ ) {
    #ifdef SENSOR3
      float to_end = vdst4(model_q1[i],model_q1[MOTsamplecount_max]) + 
                     vdst4(model_q2[i],model_q2[MOTsamplecount_max]) + 
                     vdst4(model_q3[i],model_q3[MOTsamplecount_max]);
    #else 
      float to_end = vdst4(model_q1[i],model_q1[MOTsamplecount_max]) + 
                     vdst4(model_q2[i],model_q2[MOTsamplecount_max]);
    #endif
    int TF_reconstructed = (int) ( ( a_pitch * ( ( to_end - to_end_start ) * ( to_end - to_end_start ) ) ) + pitchinitial );
    if ( to_end > to_end_start ) {
      TF_reconstructed = TF_reconstructed * -1;
    } 
    amplitude_update( amp_max );
    UpdateTF( TF_reconstructed );
    delay(1); 
  }

  amplitude_update(0);
  delaywithFIFOreset(1);
  
}

void UpdateTF( int new_pitch ) {
  new_pitch = constrain(new_pitch,PitchValueMin,PitchValueMax);
  timerSW_top = (int) timerSizeprescaler / (new_pitch * 2);
  timerAlarmWrite(timerSW, timerSW_top, true); // true = reload automatically
}

void amplitude_update( int thisamplitude ){
  thisamplitude = constrain(thisamplitude,0,amp_max);
  amp = thisamplitude;
}

void print_vector3 ( Vectors v ) {

  SERIAL_PORT.print(v.x,5); 
  SERIAL_PORT.print(F(", "));
  SERIAL_PORT.print(v.y,5); 
  SERIAL_PORT.print(F(", "));
  SERIAL_PORT.print(v.z,5); 
  SERIAL_PORT.print(F(", "));
  
}

void print_vector3ln ( Vectors v ) {

  SERIAL_PORT.print(v.x,5); 
  SERIAL_PORT.print(F(", "));
  SERIAL_PORT.print(v.y,5); 
  SERIAL_PORT.print(F(", "));
  SERIAL_PORT.println(v.z,5); 
  
}

void print_vector4 ( Vectors v ) {

  SERIAL_PORT.print(v.x,5); 
  SERIAL_PORT.print(F(", "));
  SERIAL_PORT.print(v.y,5); 
  SERIAL_PORT.print(F(", "));
  SERIAL_PORT.print(v.z,5); 
  SERIAL_PORT.print(F(", "));
  SERIAL_PORT.print(v.r,5); 
  SERIAL_PORT.print(F(", "));
  
}

void print_vector4ln ( Vectors v ) {

  SERIAL_PORT.print(v.x,5); 
  SERIAL_PORT.print(F(", "));
  SERIAL_PORT.print(v.y,5); 
  SERIAL_PORT.print(F(", "));
  SERIAL_PORT.print(v.z,5); 
  SERIAL_PORT.print(F(", "));
  SERIAL_PORT.println(v.r,5); 
  
}

void compute_motion (void) {

  // find the ROT vectors / total rotation about each axis from start (in radians)
  Sensor1_Rot = vadd( Sensor1_Rot, vscaler_mult(Sensor1_Gyro,dt) );
  Sensor2_Rot = vadd( Sensor2_Rot, vscaler_mult(Sensor2_Gyro,dt) );
  #ifdef SENSOR3
    Sensor3_Rot = vadd( Sensor3_Rot, vscaler_mult(Sensor3_Gyro,dt) );
  #endif

  #ifdef SUBTRACTGYROBIAS
    Sensor1_Gyro = vsubtract(Sensor1_Gyro,Sensor1_GyroBias); // recall that the GyroBias vectors have already had each component multipled by dt
    Sensor2_Gyro = vsubtract(Sensor2_Gyro,Sensor2_GyroBias);
    #ifdef SENSOR3
      Sensor3_Gyro = vsubtract(Sensor3_Gyro,Sensor3_GyroBias);
    #endif
  #endif 
  
  // Find quats describing sensor rotation during sample period
  Vectors q1 = rot_quat ( vscaler_mult(Sensor1_Gyro,dt), Sensor1_Xaxis, Sensor1_Yaxis, Sensor1_Zaxis );
  Vectors q2 = rot_quat ( vscaler_mult(Sensor2_Gyro,dt), Sensor2_Xaxis, Sensor2_Yaxis, Sensor2_Zaxis );
  #ifdef SENSOR3
    Vectors q3 = rot_quat ( vscaler_mult(Sensor3_Gyro,dt), Sensor3_Xaxis, Sensor3_Yaxis, Sensor3_Zaxis );
  #endif

  // update axis position relative relative to their initial position / in coordinates defined by their initial position
  Sensor1_Xaxis = qvq(q1,Sensor1_Xaxis);
  Sensor1_Yaxis = qvq(q1,Sensor1_Yaxis);
  Sensor1_Zaxis = qvq(q1,Sensor1_Zaxis);
  Sensor2_Xaxis = qvq(q2,Sensor2_Xaxis);
  Sensor2_Yaxis = qvq(q2,Sensor2_Yaxis);
  Sensor2_Zaxis = qvq(q2,Sensor2_Zaxis);
  #ifdef SENSOR3
    Sensor3_Xaxis = qvq(q3,Sensor3_Xaxis);
    Sensor3_Yaxis = qvq(q3,Sensor3_Yaxis);
    Sensor3_Zaxis = qvq(q3,Sensor3_Zaxis);
  #endif

  // Compute quats describing total sensor rotation from initial position 
  if (FIRSTSAMPLE) {
    FIRSTSAMPLE = false;
    Sensor1_Quat = q1;
    Sensor2_Quat = q2;
    #ifdef SENSOR3
      Sensor3_Quat = q3;
    #endif
  } else {
    Sensor1_Quat = quat_mult( q1, Sensor1_Quat );
    Sensor2_Quat = quat_mult( q2, Sensor2_Quat );
    #ifdef SENSOR3
      Sensor3_Quat = quat_mult( q3, Sensor3_Quat );
    #endif
  }
  
}

Vectors rot_quat ( Vectors r, Vectors ax, Vectors ay, Vectors az ) {
  ax = normalize(ax); // we must ensure this is normalized, although it should be as well (if increased speed needed, drop this step).
  ay = normalize(ay); // we must ensure this is normalized, although it should be as well (if increased speed needed, drop this step).
  az = normalize(az); // we must ensure this is normalized, although it should be as well (if increased speed needed, drop this step).
  Vectors qx = formquat(r.x,ax);
  Vectors qy = formquat(r.y,ay);
  Vectors qz = formquat(r.z,az);
  Vectors q = quat_mult( qx, quat_mult( qy, qz) );
  q = normalize4(q); // we must ensure this is normalized, although it should be as well (if increased speed needed, drop this step).
  return(q);
}

Vectors formquat ( float theta, Vectors v ) {
  Vectors q;
  float cos2theta = cosf(theta/2.0);
  float sin2theta = sinf(theta/2.0);
  q.r = cos2theta;
  q.x = sin2theta * v.x;
  q.y = sin2theta * v.y;
  q.z = sin2theta * v.z;
  return (q); 
}

Vectors quat_mult ( Vectors q1, Vectors q2 ) {
  Vectors q = vadd( vscaler_mult(q2,q1.r) , vadd( vscaler_mult(q1,q2.r) , crossp(q1,q2) ) );
  q.r = q1.r * q2.r - dotp(q1,q2); 
  return(q);
}

Vectors normalize ( Vectors v ) {
  Vectors w;
  w = vdivide(v,vmag(v));
  return(w); 
}

Vectors normalize4 ( Vectors v ) {
  Vectors w;
  w = vdivide4(v,vmag4(v));
  return(w); 
}

Vectors vscaler_mult ( Vectors thisVector1, float thisfloat ) {
  Vectors output;
  output.x = thisVector1.x * thisfloat;
  output.y = thisVector1.y * thisfloat;
  output.z = thisVector1.z * thisfloat;
  return(output);
}

float dotp ( Vectors thisVector1, Vectors thisVector2 ) {
  float dotproduct;
  dotproduct = thisVector1.x * thisVector2.x + thisVector1.y * thisVector2.y + thisVector1.z * thisVector2.z ;
  return dotproduct;
}

Vectors crossp ( Vectors thisVector1, Vectors thisVector2 ) {
  Vectors crossproduct;
  crossproduct.x = thisVector1.y * thisVector2.z - thisVector1.z * thisVector2.y ;
  crossproduct.y = thisVector1.z * thisVector2.x - thisVector1.x * thisVector2.z ;
  crossproduct.z = thisVector1.x * thisVector2.y - thisVector1.y * thisVector2.x ;
  return(crossproduct);
}

float vdst ( Vectors thisVector1, Vectors thisVector2 ) {
  float x = thisVector1.x - thisVector2.x;
  float y = thisVector1.y - thisVector2.y;
  float z = thisVector1.z - thisVector2.z;
  float distance = sqrtf ( x*x + y*y + z*z );
  return distance;
}

float vdst4 ( Vectors thisVector1, Vectors thisVector2 ) {
  float x = thisVector1.x - thisVector2.x;
  float y = thisVector1.y - thisVector2.y;
  float z = thisVector1.z - thisVector2.z;
  float r = thisVector1.r - thisVector2.r;
  float distance = sqrtf ( x*x + y*y + z*z + r*r );
  return distance;
}

float vdstSq4 ( Vectors thisVector1, Vectors thisVector2 ) {
  float x = thisVector1.x - thisVector2.x;
  float y = thisVector1.y - thisVector2.y;
  float z = thisVector1.z - thisVector2.z;
  float r = thisVector1.r - thisVector2.r;
  float distance = x*x + y*y + z*z + r*r ;
  return distance;
}

float vmag ( Vectors thisVector ) {
  float MagV;
  MagV = sqrtf ( thisVector.x * thisVector.x + thisVector.y * thisVector.y + thisVector.z * thisVector.z );
  return MagV;
}

float vmag4 ( Vectors thisVector ) {
  float MagV;
  MagV = sqrtf ( thisVector.r * thisVector.r + thisVector.x * thisVector.x + thisVector.y * thisVector.y + thisVector.z * thisVector.z );
  return MagV;
}

float vmagSqrd ( Vectors thisVector ) {
  float MagV;
  MagV = thisVector.x * thisVector.x + thisVector.y * thisVector.y + thisVector.z * thisVector.z ;
  return MagV;
}

Vectors vdivide ( Vectors v, float r ) {
  Vectors w;
  w.x = v.x /r;
  w.y = v.y /r;
  w.z = v.z /r;
  return(w);
}

Vectors vdivide4 ( Vectors v, float r ) {
  Vectors w;
  w.r = v.r /r;
  w.x = v.x /r;
  w.y = v.y /r;
  w.z = v.z /r;
  return(w);
}

Vectors vsubtract ( Vectors v1, Vectors v2 ) {
  Vectors output;
  output.x = v1.x - v2.x;
  output.y = v1.y - v2.y;
  output.z = v1.z - v2.z;
  return output;
}

Vectors vadd ( Vectors v1, Vectors v2 ) {
  Vectors output;
  output.x = v1.x + v2.x;
  output.y = v1.y + v2.y;
  output.z = v1.z + v2.z;
  return output;
}

Vectors vrunning_avg ( Vectors thisVector1, Vectors thisVector2, int thisNum ) {
  Vectors ravg;
  ravg.x = (thisVector1.x * thisNum + thisVector2.x) / (1 + thisNum);
  ravg.y = (thisVector1.y * thisNum + thisVector2.y) / (1 + thisNum);
  ravg.z = (thisVector1.z * thisNum + thisVector2.z) / (1 + thisNum);
  return(ravg);
}

Vectors qvq ( Vectors thisQuat, Vectors thisPosition ) {
  // Fewer computations than the other definition
  // Also, note that we're assuming our quaternions are unit quaternions (they should be, or should be very close). 

  thisPosition.r = 0;
  float qv_r = thisQuat.r * thisPosition.r - thisQuat.x * thisPosition.x - thisQuat.y * thisPosition.y - thisQuat.z * thisPosition.z ;
  float qv_x = thisQuat.r * thisPosition.x + thisQuat.x * thisPosition.r + thisQuat.y * thisPosition.z - thisQuat.z * thisPosition.y ; 
  float qv_y = thisQuat.r * thisPosition.y - thisQuat.x * thisPosition.z + thisQuat.y * thisPosition.r + thisQuat.z * thisPosition.x ;
  float qv_z = thisQuat.r * thisPosition.z + thisQuat.x * thisPosition.y - thisQuat.y * thisPosition.x + thisQuat.z * thisPosition.r ; 

  // This is the same operation as above, expect the sign is flipped everywhere we have a non-real quat component (since the second q in qvq is the inverse of q). 
  //float qvq_r = qv_r * thisQuat.r + qv_x * thisQuat.x + qv_y * thisQuat.y + qv_z * thisQuat.z ; // no point in computing r value of the output
  float qvq_x = qv_r * thisQuat.x * -1.0 + qv_x * thisQuat.r - qv_y * thisQuat.z + qv_z * thisQuat.y ;
  float qvq_y = qv_r * thisQuat.y * -1.0 + qv_x * thisQuat.z + qv_y * thisQuat.r - qv_z * thisQuat.x ;
  float qvq_z = qv_r * thisQuat.z * -1.0 - qv_x * thisQuat.y + qv_y * thisQuat.x + qv_z * thisQuat.r ;

  Vectors thisFinalPosition;
  thisFinalPosition.r = 0; // qvq_r ; // physically meaningful vectors have r = 0
  thisFinalPosition.x = qvq_x ;
  thisFinalPosition.y = qvq_y ;
  thisFinalPosition.z = qvq_z ;

  return(thisFinalPosition);
}

void FetchReadingsSensor (ICM_20948_SPI &myICM, Vectors &storeAccReadingInThisVector, Vectors &storeGyroReadingInThisVector) { 
  
  myICM.getAGMT();            // get the A, G, M, and T readings
  // Scaled data (2000dps gyro, 16g accel range; for 4g, use 8192)
  storeAccReadingInThisVector.x = myICM.agmt.acc.axes.x * Acc_multiple; // convert first to g by dividing by 8192, then to m/s^2 by multiplying by 9.80665, i.e. (myICM.agmt.acc.axes.x / 2048.0) * 9.80665
  storeAccReadingInThisVector.y = myICM.agmt.acc.axes.y * Acc_multiple;
  storeAccReadingInThisVector.z = myICM.agmt.acc.axes.z * Acc_multiple;
  storeGyroReadingInThisVector.x = myICM.agmt.gyr.axes.x * Gyro_multiple; // convert to degrees/s by dividing by 16.4, then to radians by multiplying by * (PI/180.0), i.e. (myICM.agmt.gyr.axes.x / 16.4) * (PI/180.0)
  storeGyroReadingInThisVector.y = myICM.agmt.gyr.axes.y * Gyro_multiple;
  storeGyroReadingInThisVector.z = myICM.agmt.gyr.axes.z * Gyro_multiple; 
  /* storeMagReadingInThisVector.x = myICM.agmt.mag.axes.x;
  storeMagReadingInThisVector.y = myICM.agmt.mag.axes.y;
  storeMagReadingInThisVector.z = myICM.agmt.mag.axes.z; */
 
}

void initializeSensor_SPI (ICM_20948_SPI &myICM, int thisPin, int thisNum ) {
  bool initialized = false;
  while (!initialized) {

    myICM.begin(thisPin, SPI_PORT, SPI_FREQ); // Here we are using the user-defined SPI_FREQ as the clock speed of the SPI bus

    SERIAL_PORT.print(F("Initialization of sensor "));
    SERIAL_PORT.print(thisNum,1);
    SERIAL_PORT.print(F(" returned: "));
    SERIAL_PORT.println(myICM.statusString());
    if (myICM.status != ICM_20948_Stat_Ok)
    {
      SERIAL_PORT.println("Trying again...");
      delay(500);
    }
    else
    {
      initialized = true;
    }
  }
  SERIAL_PORT.print(F("Device "));
  SERIAL_PORT.print(thisNum,1);
  SERIAL_PORT.println(F(" connected!"));
}

void configureInterrupts (ICM_20948_SPI &myICM) {
  // Now we're going to set up interrupts. There are a lot of options, but for this test we're just configuring the interrupt pin and enabling interrupts to tell us when new data is ready
  /*
    ICM_20948_Status_e  cfgIntActiveLow         ( bool active_low );
    ICM_20948_Status_e  cfgIntOpenDrain         ( bool open_drain );
    ICM_20948_Status_e  cfgIntLatch             ( bool latching );                          // If not latching then the interrupt is a 50 us pulse
    ICM_20948_Status_e  cfgIntAnyReadToClear    ( bool enabled );                           // If enabled, *ANY* read will clear the INT_STATUS register. So if you have multiple interrupt sources enabled be sure to read INT_STATUS first
    ICM_20948_Status_e  cfgFsyncActiveLow       ( bool active_low );
    ICM_20948_Status_e  cfgFsyncIntMode         ( bool interrupt_mode );                    // Can ue FSYNC as an interrupt input that sets the I2C Master Status register's PASS_THROUGH bit
    ICM_20948_Status_e  intEnableI2C            ( bool enable );
    ICM_20948_Status_e  intEnableDMP            ( bool enable );
    ICM_20948_Status_e  intEnablePLL            ( bool enable );
    ICM_20948_Status_e  intEnableWOM            ( bool enable );
    ICM_20948_Status_e  intEnableWOF            ( bool enable );
    ICM_20948_Status_e  intEnableRawDataReady   ( bool enable );
    ICM_20948_Status_e  intEnableOverflowFIFO   ( uint8_t bm_enable );
    ICM_20948_Status_e  intEnableWatermarkFIFO  ( uint8_t bm_enable );
 */
  myICM.cfgIntActiveLow(true);  // Active low to be compatible with the breakout board's pullup resistor
  myICM.cfgIntOpenDrain(false); // Push-pull, though open-drain would also work thanks to the pull-up resistors on the breakout
  myICM.cfgIntLatch(true);      // Latch the interrupt until cleared; Not sure why, but with high speed DMP does not work if we latch; must set to false. 
  SERIAL_PORT.print(F("cfgIntLatch returned: "));
  SERIAL_PORT.println(myICM.statusString());

  myICM.intEnableRawDataReady(true); // enable interrupts on the DMP
  SERIAL_PORT.print(F("intEnableRawDataReady returned: "));
  SERIAL_PORT.println(myICM.statusString());

  //  // Note: weirdness with the Wake on Motion interrupt being always enabled.....
  //  uint8_t zero_0 = 0xFF;
  //  ICM_20948_execute_r( &myICM._device, AGB0_REG_INT_ENABLE, (uint8_t*)&zero_0, sizeof(uint8_t) );
  //  SERIAL_PORT.print("INT_EN was: 0x"); SERIAL_PORT.println(zero_0, HEX);
  //  zero_0 = 0x00;
  //  ICM_20948_execute_w( &myICM._device, AGB0_REG_INT_ENABLE, (uint8_t*)&zero_0, sizeof(uint8_t) );
}

void configureSensor (ICM_20948_SPI &myICM) {
// Here we are doing a SW reset to make sure the device starts in a known state
  myICM.swReset();
  if (myICM.status != ICM_20948_Stat_Ok)
  {
    SERIAL_PORT.print(F("Software Reset returned: "));
    SERIAL_PORT.println(myICM.statusString());
  }
  delay(250);

  // Now wake the sensor up
  myICM.sleep(false);
  myICM.lowPower(false);

  // The next few configuration functions accept a bit-mask of sensors for which the settings should be applied.

  // Set Gyro and Accelerometer to a particular sample mode
  // options: ICM_20948_Sample_Mode_Continuous
  //          ICM_20948_Sample_Mode_Cycled
  myICM.setSampleMode((ICM_20948_Internal_Acc | ICM_20948_Internal_Gyr), ICM_20948_Sample_Mode_Continuous);
  SERIAL_PORT.print(F("setSampleMode returned: "));
  SERIAL_PORT.println(myICM.statusString());

//  ICM_20948_smplrt_t mySmplrt;
//  mySmplrt.g = 54;
//  myICM.setSampleRate(ICM_20948_Internal_Gyr, mySmplrt);
//  SERIAL_PORT.print(F("setSampleRate returned: "));
//  SERIAL_PORT.println(myICM.statusString());

  // Set full scale ranges for both acc and gyr
  ICM_20948_fss_t myFSS; // This uses a "Full Scale Settings" structure that can contain values for all configurable sensors

  myFSS.a = gpm16; // (ICM_20948_ACCEL_CONFIG_FS_SEL_e)
                  // gpm2
                  // gpm4
                  // gpm8
                  // gpm16

  myFSS.g = dps2000; // (ICM_20948_GYRO_CONFIG_1_FS_SEL_e)
                    // dps250
                    // dps500
                    // dps1000
                    // dps2000

  myICM.setFullScale((ICM_20948_Internal_Acc | ICM_20948_Internal_Gyr), myFSS);
  if (myICM.status != ICM_20948_Stat_Ok)
  {
    SERIAL_PORT.print(F("setFullScale returned: "));
    SERIAL_PORT.println(myICM.statusString());
  }

  // Set gyro sample rate divider with GYRO_SMPLRT_DIV
  // Set accel sample rate divider with ACCEL_SMPLRT_DIV_2
  ICM_20948_smplrt_t mySmplrt;
  //mySmplrt.g = 19; // ODR is computed as follows: 1.1 kHz/(1+GYRO_SMPLRT_DIV[7:0]). 19 = 55Hz. InvenSense Nucleo example uses 19 (0x13).
  //mySmplrt.a = 19; // ODR is computed as follows: 1.125 kHz/(1+ACCEL_SMPLRT_DIV[11:0]). 19 = 56.25Hz. InvenSense Nucleo example uses 19 (0x13).
  #ifdef RUNHALFSPEED
    mySmplrt.g = 4; // 225Hz
    mySmplrt.a = 4; // 225Hz
  #endif
  //mySmplrt.g = 1; // 550Hz // I would have assumed this should be 2, but with 2, only get 377 or 378 updates/sec; with 1, get 565, 566 --- Is it because this number is an exponent on divider, e.g. 1100 / 2^x?
  //mySmplrt.a = 1; // 562.5Hz // I would have assumed this should be 2, but with 2, only get 377 or 378 updates/sec; with 1, get 565, 566
  //mySmplrt.g = 8; // 112Hz
  //mySmplrt.a = 8; // 112Hz
  #ifdef RUNFULLSPEED 
    mySmplrt.g = 0; // 1100Hz 
    mySmplrt.a = 0; // 1125Hz
  #endif
  myICM.setSampleRate((ICM_20948_Internal_Acc | ICM_20948_Internal_Gyr), mySmplrt); 
  SERIAL_PORT.print(F("setSampleRate (Acc and Gyro) returned: "));
  SERIAL_PORT.println(myICM.statusString());

  // ... don't want or need the low-pass filter (we want to detect fast changes), at least, crank the filter down as low as it goes (499bw/376.5)
  // Assume these filters are running averages of some sort, e.g. nyquist bandwidth of 265Hz means a running average (of faster samples) is returned 265 times/sec
  // Set up Digital Low-Pass Filter configuration
  ICM_20948_dlpcfg_t myDLPcfg;    // Similar to FSS, this uses a configuration structure for the desired sensors
  myDLPcfg.a = acc_d473bw_n499bw; // (ICM_20948_ACCEL_CONFIG_DLPCFG_e)
                                  // acc_d246bw_n265bw        - means 3db bandwidth is 246 hz and nyquist bandwidth is 265 hz
                                  // acc_d111bw4_n136bw    *
                                  // acc_d50bw4_n68bw8
                                  // acc_d23bw9_n34bw4
                                  // acc_d11bw5_n17bw
                                  // acc_d5bw7_n8bw3        - means 3 db bandwidth is 5.7 hz and nyquist bandwidth is 8.3 hz
                                  // acc_d473bw_n499bw

  myDLPcfg.g = gyr_d361bw4_n376bw5; // (ICM_20948_GYRO_CONFIG_1_DLPCFG_e)
                                    // gyr_d196bw6_n229bw8   
                                    // gyr_d151bw8_n187bw6   *
                                    // gyr_d119bw5_n154bw3
                                    // gyr_d51bw2_n73bw3
                                    // gyr_d23bw9_n35bw9
                                    // gyr_d11bw6_n17bw8
                                    // gyr_d5bw7_n8bw9
                                    // gyr_d361bw4_n376bw5

  myICM.setDLPFcfg((ICM_20948_Internal_Acc | ICM_20948_Internal_Gyr), myDLPcfg);
  if (myICM.status != ICM_20948_Stat_Ok)
  {
    SERIAL_PORT.print(F("setDLPcfg returned: "));
    SERIAL_PORT.println(myICM.statusString());
  }

  // Choose whether or not to use DLPF
  // Here we're also showing another way to access the status values, and that it is OK to supply individual sensor masks to these functions
  ICM_20948_Status_e accDLPEnableStat = myICM.enableDLPF(ICM_20948_Internal_Acc, false);
  ICM_20948_Status_e gyrDLPEnableStat = myICM.enableDLPF(ICM_20948_Internal_Gyr, false);
  SERIAL_PORT.print(F("Enable/disable DLPF for Accelerometer returned: "));
  SERIAL_PORT.println(myICM.statusString(accDLPEnableStat));
  SERIAL_PORT.print(F("Enable/disable DLPF for Gyroscope returned: "));
  SERIAL_PORT.println(myICM.statusString(gyrDLPEnableStat));

  // Choose whether or not to start the magnetometer
  //  myICM.startupMagnetometer();
  //  if (myICM.status != ICM_20948_Stat_Ok)
  //  {
  //    SERIAL_PORT.print(F("startupMagnetometer returned: "));
  //    SERIAL_PORT.println(myICM.statusString());
  //  }
}

void delaywithFIFOreset(int milli) {
  delay(milli);
  myICM1.resetFIFO(); // Need to keep FIFO current?
  myICM2.resetFIFO();
  #ifdef SENSOR3
    myICM3.resetFIFO();
  #endif
}
