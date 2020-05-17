#include <TinyGPS++.h>
#include <SoftwareSerial.h>
#include <SD.h>
#include <SPI.h>

static const int RXPin = 2, TXPin = 3;
static const uint32_t GPSBaud = 9600;

int ID = 0;

File dataFile;

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
}

void loop(){
  // This sketch displays information every time a new sentence is correctly encoded.
  while (ss.available() > 0){
    gps.encode(ss.read());
    if (gps.location.isUpdated()){
      Serial.print(ID);
      Serial.print(",");
      Serial.print(gps.location.lat(), 6);
      Serial.print(",");
      //Serial.print(" LON= "); 
      Serial.print(gps.location.lng(), 6);
      Serial.print(",");
      //Serial.print(" ALT= "); 
      Serial.print(gps.altitude.meters());
      Serial.print(",");
      //Serial.print(" Time= "); 
      Serial.print(gps.time.value());
      Serial.print(",");
      //Serial.print(" Date= "); 
      Serial.print(gps.date.value());
      Serial.print(",");
      //Serial.print(" Sat= "); 
      Serial.print(gps.satellites.value());
      Serial.print(",");
      //Serial.print(" Speed= "); 
      Serial.print(gps.speed.mps());  
      Serial.println("");  
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
      dataFile.println("");    
      dataFile.close();
    }
  }
}
