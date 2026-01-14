2026-01-14

EN:
======================================================================
GPS SATELLITE POSITION DETERMINED BY THE RECEIVER FOR GIVEN GPS TIME
USING ORBITAL PARAMETERS IN ACCORDANCE WITH THE IS‑GPS‑200
SPECIFICATION
======================================================================

The program for computing the position of a GPS satellite in the ECEF
coordinate system based on sample orbital parameters (ephemeris),
ephemeris validity interval and GPS time provided as input. The
program is not intended for post‑processing applications.

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
======================================================================
POZYCJA SATELITY GPS WYZNACZANA PRZEZ ODBIORNIK DLA DANEGO CZASU GPS
Z WYKORZYSTANIEM PARAMETRÓW ORBITY ZGODNIE ZE SPECYFIKACJĄ IS‑GPS‑200
======================================================================

Program wyznacza pozycję satelity GPS w układzie ECEF na podstawie
przykładowych parametrów orbity tzw. efemerydy, okresu ważności
efemerydy oraz podanego czasu GPS. Program nie jest przeznaczony do
zastosowań w postprocessingu.


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