2026-01-14

EN:
======================================================================
GPS SATELLITE POSITION FOR GIVEN GPS TIME USING ORBITAL PARAMETERS
======================================================================

The program for determining the position of a GPS satellite in the ECEF
system based on sample orbital parameters (ephemeris), the ephemeris
validity interval, and a given GPS time defined as (w, tow), where w is
the GPS week and tow is the time of week.

Input
-----
- GPS Ephemeris
- ephemeris validity interval
- GPS Time

Output
------
- ECEF satellite position

Print of run
------------
```
Entered GPS time            : 2024 03 07 22 00 30
Ephemeris reference GPS time: 2024 03 07 22 00 00
Diference                   :            00 00 30

ECEF satellite position [m]:
X = 22151566.575334515
Y = 13275548.286060918
Z =  7260529.645377433
```

PL:
=======================================================================
POZYCJA SATELITY GPS DLA DANEGO CZASU GPS Z WYKORZYSTANIEM PARAMETRÓW
ORBITY
=======================================================================

Program do wyznaczania pozycji satelity GPS w układzie ECEF na
podstawie przykładowych parametrów orbity tzw. efemerydy, okresu
ważności efemerydy oraz podanego czasu GPS zdefiniowanego jako (w,tow),
gdzie w jest tygodniem GPS a tow jest czasem tygodnia.

Wejście
-------
- efemeryda GPS
- okres ważności efemerydy
- czas GPS

Wyjście
-------
- pozycja satelity w ECEF

Wydruk uruchomienia
-------------------
```
Entered GPS time            : 2024 03 07 22 00 30
Ephemeris reference GPS time: 2024 03 07 22 00 00
Diference                   :            00 00 30

ECEF satellite position [m]:
X = 22151566.575334515
Y = 13275548.286060918
Z =  7260529.645377433
```