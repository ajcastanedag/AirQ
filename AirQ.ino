// GPS MODULE 
#include <TinyGPS++.h>
#include <SoftwareSerial.h>
// SD MODULE 
#include <SD.h>
#include <SPI.h>
// BME 280 MODULE 
#include "Seeed_BME280.h"
#include <Wire.h>

// MQ135
#include "MQ135.h"
#define RZERO 76.63

static const int RXPin = 2, TXPin = 3;
static const uint32_t GPSBaud = 9600;

int ID = 0;
float BMEaltitude;

const int ANALOGPIN=0;

File dataFile;

BME280 bme280;

MQ135 gasSensor = MQ135(ANALOGPIN);

// The TinyGPS++ object
TinyGPSPlus gps;

// The serial connection to the GPS device
SoftwareSerial ss(RXPin, TXPin);

void setup(){
  Serial.begin(9600);
  ss.begin(GPSBaud);
  if (!SD.begin(10)) {
    Serial.println("SD Fail"); 
  }else{Serial.println("SD OK");}
  if (!bme280.init()) {
        Serial.println("BME280 error!");
    }else{Serial.println("BME280 OK");}
}

void loop(){
  // This sketch displays information every time a new sentence is correctly encoded.
  if (ss.available() > 0){
    gps.encode(ss.read());
    if (gps.location.isUpdated() && gps.satellites.value() > 0){
      ID = ID + 1;
      // Declare and open txt file in SD
      dataFile = SD.open("Test.txt", FILE_WRITE);
      dataFile.print(ID);
      dataFile.print(",");
      dataFile.print(gps.location.lat(), 6);
      dataFile.print(",");
      dataFile.print(gps.location.lng(), 6);
      dataFile.print(",");
      dataFile.print(gps.altitude.meters());
      dataFile.print(",");
      dataFile.print(gps.time.value());
      dataFile.print(",");
      dataFile.print(gps.date.value());
      dataFile.print(",");
      dataFile.print(gps.satellites.value());
      dataFile.print(",");
      dataFile.print(gps.speed.mps());  
      dataFile.print(",");
      dataFile.print(bme280.getTemperature(),1);
      dataFile.print(",");
      dataFile.print(bme280.calcAltitude(bme280.getPressure()));
      dataFile.print(",");
      dataFile.print(bme280.getHumidity());
      dataFile.print(",");
      dataFile.print(gasSensor.getPPM());
      dataFile.println("");
      dataFile.close();
      Serial.print(gps.satellites.value());
      Serial.print(" - ");
      Serial.print(ID);
      Serial.println("");
    }
  }
}
