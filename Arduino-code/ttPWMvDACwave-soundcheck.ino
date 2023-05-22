/*
  Setup base wave form @ 152,380 Hz using the ESP32's hardware PWM
      152,380 Hz is the fastest oscilation its hardware PWM can achieve with at least 9 bit of resolution
        Get this with: clk_src = LEDC_APB_CLK (80 MHz).
        ... then duty resolution is integer (log 2 (LEDC_APB_CLK / frequency))
      see: 
        https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/ledc.html
        https://github.com/espressif/esp-idf/blob/5893797bf068ba6f72105fff289ead370b4591a3/examples/peripherals/ledc/ledc_basic/main/ledc_basic_example_main.c
  Initial code to get PWM working from: https://randomnerdtutorials.com/esp32-pwm-arduino-ide/
  Note: We really want this PWM to run in LEDC_HIGH_SPEED_MODE; the code from the esp-idf gives this control, but not the code for arduino;
    Currently; assuming arduino puts PWM into high-speed mode by default
*/

//#define DACwave // uncomment to use DAC to generate base pulse wave instead of ttPWM

const int update_rate = 1000; // Controls both sound updates and when to process sensor data/ update motion models

// Variables controlling base pulse wave production / volume control
#ifdef DACwave
const int soundPin = A1;  // DAC is set to use DAC1, which is A1; DAC2 is A0
const int amp_max = 255; // DAC is 8-bit, so only 256 levels
#else 
const int soundPin = 25; 
const int base_wave_freq = 152380; // highest availabe with 9 bit resolution is 152380
const int pwm_Channel = 0;
const int pwm_resolution = (int) log2( 80000000 / base_wave_freq ) ;
const int amp_max = pow(2,pwm_resolution) - 1; // for 9 bit resolution: 2^9 - 1 = 511; for 8 bit 2^8 - 1 = 255
#endif
volatile int amp = (int) amp_max * 0.5;
// setting the timer for generating hearable oscilation 
const int pitch_initial = 440;
volatile int pitch = pitch_initial;
hw_timer_t * timerSW = NULL;
portMUX_TYPE timerSWMux = portMUX_INITIALIZER_UNLOCKED;
const int timerSWprescaler = 2; // Timers are 80Mhz; counters seem to be at least 32bit, so, don't need to prescale down low
const long timerSize = 80000000;
const int timerSizeprescaler = timerSize/timerSWprescaler;
volatile int timerSW_top = (int) timerSizeprescaler / (pitch * 2); // controls frequency of hearable oscilation
volatile bool up = true;

int PitchValueMax = 6000;
int MOTsamplecount_saved = 1000;
const float maxallowederror_ampSon = 100.0 ; // amplitude is controlled by POS vector error; values in mm, typically 10-100
const float h_ampWeber = maxallowederror_ampSon ; 
const float k_ampWeber = amp_max ; 
const float a_ampWeber = - k_ampWeber / ( h_ampWeber * h_ampWeber ) ; 
// this time the error is added to the audio quality, not substracted, so we want the vertex at (0,pitchinitial) and the function to intersect the point (MOTsamplecount_saved, maxpitch).
const float h_pitchWeber = 0 ; 
const float k_pitchWeber = pitch_initial ; 
float a_pitchWeber = (PitchValueMax - pitch_initial) / ( (MOTsamplecount_saved - 1.0) * (MOTsamplecount_saved - 1.0) ) ;  // Must be reset below when model is sampled

int MOTsamplecount = 0;
float total_error = 1.0;
float error_weber_Pos = 1.0;
int TF = 0;

void update_pitch ( int new_pitch ) {
  timerSW_top = (int) timerSizeprescaler / (new_pitch * 2);
  timerAlarmWrite(timerSW, timerSW_top, true); // true = reload automatically
}

// interrupt handlers for timers generating hearable oscilation: 
void IRAM_ATTR onTimerSW() {
  portENTER_CRITICAL_ISR(&timerSWMux);
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
  portEXIT_CRITICAL_ISR(&timerSWMux);
}

// Variables for the timer controlling sound updates / motion processing
hw_timer_t * timerSU = NULL;
portMUX_TYPE timerSUMux = portMUX_INITIALIZER_UNLOCKED;
const int timerSU_top = (int) timerSizeprescaler / update_rate; 
volatile bool update_now = false;

// interrupt handler for timer generating sound updates / motion processing: 
void IRAM_ATTR onTimerSU() {
  portENTER_CRITICAL_ISR(&timerSWMux);
  update_now = true;
  portEXIT_CRITICAL_ISR(&timerSWMux);
}

long time1 = 0;
long time2 = 0;
int state = 1;
int maxstate = 10;
int statelength = 5 * 1000;
 
void setup(){

  Serial.begin(115200);

  // Setup base pulse wav
  #ifdef DACwave
    // print notice that DAC is used; nothing else to run
  #else
    // print notice that ttPWM is beeing used
    ledcSetup(pwm_Channel, base_wave_freq, pwm_resolution); // configure PWM functionalitites
    ledcAttachPin(soundPin, pwm_Channel); // attach the channel to the pin generating the wave
  #endif
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

  amp = amp_max;

  time1 = millis();
}
 
void loop(){  

  time2 = millis();
  if ( time2 - time1 > statelength ) {
    time1 = time2; 
    state++;
    if ( state > maxstate ) state = 1;
  }

  if (update_now) {

    if ( state == 1 ) {
      pitch = 440;
      update_pitch(pitch);
      amp = amp_max;
    } else if ( state == 2 ) {
      pitch = 441;
      update_pitch(pitch);
      amp = amp_max;
    } else if ( state == 3 ) {
      pitch = 442; 
      update_pitch(pitch);
      amp = amp_max;
    } else if ( state == 4) {
      pitch = 443;
      update_pitch(pitch);
      amp = amp_max;
    } else if ( state == 5 ) {
      pitch = 880;
      update_pitch(pitch);
      amp = amp_max;
    } else if ( state == 6 ) {
      pitch = 880;
      update_pitch(pitch);
      amp = (int) amp_max * 0.67;
    } else if ( state == 7 ) {
      pitch = 880;
      update_pitch(pitch);
      amp = (int) amp_max * 0.33;
    } else if ( state == 8 ) {
      pitch = 2000;
      update_pitch(pitch);
      amp = (int) amp_max;
    } else if ( state == 9 ) {
      pitch = 4000;
      update_pitch(pitch);
      amp = (int) amp_max;
    } else if ( state == 10 ) {
      pitch = 6000;
      update_pitch(pitch);
      amp = (int) amp_max;
    }

    Serial.print("pitch: ");
    Serial.print(pitch);
    Serial.print(", amp: ");
    Serial.println(amp);

    // Simple increase in pitch and vol
//    if ( pitch < PitchValueMax ) pitch += 1;
//    else pitch = pitch_initial;
//    update_pitch(pitch);
//    if ( amp < 5 ) amp = amp_max;
//    else amp -= 1;

    // increase pitch and vol with heuristic weber scaling
//    if ( total_error < maxallowederror_ampSon ) total_error += 0.1;
//    else total_error = 0;
//    error_weber_Pos = WeberError_Amplitude(total_error);
//    if ( MOTsamplecount < MOTsamplecount_saved ) MOTsamplecount += 1;
//    else MOTsamplecount = 0;
//    // Compute sound update
//    amplitude_update( (int) ( amp_max - error_weber_Pos ) );
//    TF = (int) WeberError_Pitch(MOTsamplecount); // It's not "error", we just insert the sample count, but that's what the function is called due to past programming
//    update_pitch(TF);

    update_now = false;
  }
  
}

void amplitude_update( int thisamplitude ){
  thisamplitude = constrain(thisamplitude,0,amp_max);
  amp = thisamplitude;
}

float WeberError_Amplitude ( float thisInput ) { 
// If we sonify error by subtracting the raw value, we get a linear relationship between error and volume. But by Weber's law, we want something more like a quadratic relationship. 
//  So, this function adjusts the error to be subtracted so the volume response behaves better to our ears. 
  float err;
  if ( thisInput <= maxallowederror_ampSon ) {
    err = a_ampWeber * ( ( thisInput - h_ampWeber ) * ( thisInput - h_ampWeber ) ) + k_ampWeber ; 
  }
  else { // Input error should always be zero or positive, never negative. 
    err = amp_max ; 
  }
  return err;
}

float WeberError_Pitch ( float thisInput ) { 
  float err;
  thisInput = abs (thisInput); // make sure we're not negative!
  if ( thisInput <= MOTsamplecount_saved ) {
    err = a_pitchWeber * ( ( thisInput - h_pitchWeber ) * ( thisInput - h_pitchWeber ) ) + k_pitchWeber ; 
  }
  else { // the input is always zero or positive, never negative. 
    err = PitchValueMax ; 
  }
  return err;
}
