2026-01-16

EN:
======================================================================
SELECTION OF GPS NAVIGATION RECORD FROM RINEX 3.04 NAVIGATION FILE FOR
THE OBSERVATION TIME
======================================================================

The program selects a navigation record containing ephemeris from a
RINEX 3.04 navigation file for a given a GPS satellite and observation
time.  The main selection criterion is the ephemeris of a healthy
satellite with the nearest (week, toe) and the maximum IODE, for which
the observation time lies within the fitInterval.

Input
-----
  - RINEX 3.04 navigation file name
  - GPS observation time (receiver time of signal reception)
  - satellite number

                                       
Output
------
  - navigation record with ephemeris
                                       
Print of run
------------
```
Observation time: 2025 08 02 01 00 01.5
  Ephemeris time: 2025 08 02 02 00 00
PRN:  6      toc: 2025 08 02 02 00 00
af0:         -4.722196608782E-4
af1:        -1.432454155292E-11
af2:           0.000000000000E0

iode:         69
crs:          -2.368750000000E1
deltaN:       3.821944913632E-9
m0:           -2.959416369262E0

cuc:         -1.283362507820E-6
e:            3.335768124089E-3
cus:          4.727393388748E-6
sqrtA:         5.153617370605E3

toe:           5.256000000000E5
cic:          8.940696716309E-8
omega0:       6.818382481570E-1
cis:          4.470348358154E-8

i0:           9.884960063693E-1
crc:           3.054062500000E2
omega:       -6.694838801080E-1
omegaDot:    -7.833540584123E-9

iDot:       -1.753644474903E-10
skipped
week:       2377
skipped

skipped
svHealth:      0
skipped
iodc:         69

ttom:          5.184000000000E5
fitInterval:     4
----------------------------------
```

PL:
======================================================================
WYBÓR REKORDU NAWIGACYJNEGO GPS Z PLIKU NAWIGACYJNEGO RINEX 3.04 DLA
CZASU OBSERWACJI
======================================================================

Program wybiera rekord nawigacyjny zawierający efemerydę z pliku
nawigacyjnego RINEX 3.04 dla danego satelity GPS i czasu
obserwacji. Głównym kryterium wyboru jest efemeryda dla sprawnego
satelity z najbliższym (week, toe) i maksymalnym IODE, dla której czas
obserwacji mieści się w fitInterval.

Wejście
-------
  - nazwa pliku nawigacyjnego RINEX 3.04
  - czas GPS obserwacji (czas odbiornika odbioru sygnału)
  - numer satelity GPS
                                       
Wyjście
------
  - rekord nawigacyjny z efemerydą
                                       
Wydruk uruchomienia
-------------------
```
Observation time: 2025 08 02 01 00 01.5
  Ephemeris time: 2025 08 02 02 00 00
PRN:  6      toc: 2025 08 02 02 00 00
af0:         -4.722196608782E-4
af1:        -1.432454155292E-11
af2:           0.000000000000E0

iode:         69
crs:          -2.368750000000E1
deltaN:       3.821944913632E-9
m0:           -2.959416369262E0

cuc:         -1.283362507820E-6
e:            3.335768124089E-3
cus:          4.727393388748E-6
sqrtA:         5.153617370605E3

toe:           5.256000000000E5
cic:          8.940696716309E-8
omega0:       6.818382481570E-1
cis:          4.470348358154E-8

i0:           9.884960063693E-1
crc:           3.054062500000E2
omega:       -6.694838801080E-1
omegaDot:    -7.833540584123E-9

iDot:       -1.753644474903E-10
skipped
week:       2377
skipped

skipped
svHealth:      0
skipped
iodc:         69

ttom:          5.184000000000E5
fitInterval:     4
----------------------------------
```