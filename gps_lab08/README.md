2026-04-01

EN
======================================================================
SKY PLOT OF GPS SATELLITE TRAJECTORIES FROM A RINEX 3.04 NAVIGATION
FILE FOR OBSERVATION TIMES AND SATELLITES FROM A RINEX 3.04
OBSERVATION FILE
======================================================================

The program generates a sky plot of computed GPS satellite
trajectories as an SVG file, using data from the RINEX navigation file
for times and satellites from the RINEX observation file. RINEX files
in version 3.04. A sky plot is a polar diagram showing the satellite's
azimuth (0–360° angle from north to side) and elevation (0–90° angle
from the horizon upwards) relative to the observer's position. Each
satellite trajectory is drawn as a sequence of time intervals
corresponding to navigation records (ephemerides), with a PRN label, a
direction arrow, and colors derived from the fitInterval field in the
RINEX file.

Input (to modify directly in the source code)
-----
  - ECEF coordinates of observer position
  - RINEX navigation file name
  - RINEX observation file name
  - plot title

Output
------
  sky plot SVG file navSkyplotForObsTimes.svg

Print of run
------------
```
Total observation records:       651
Total observations:             7161
Number of observations
without attached ephemerides:      0
```

  ![sky plot example](navSkyplotForObsTimes.svg)


PL
======================================================================
WYKRES NIEBA Z TRAJEKTORIAMI SATELITÓW GPS Z PLIKU NAWIGACYJNEGO RINEX
3.04 DLA CZASÓW OBSERWACJI I SATELITÓW Z PLIKU OBSERWACYJNEGO RINEX
3.04
======================================================================

Program generuje wykres nieba obliczonych trajektorii satelitów GPS
jako plik SVG, korzystając z danych z pliku nawigacyjnego RINEX dla
czasów i satelitów z pliku obserwacyjnego RINEX. Pliki RINEX w wersji
3.04.  Wykres nieba (sky plot) to diagram biegunowy przedstawiający
azymut satelity (0–360° kąt od kierunku północnego w bok) oraz
elewację (0–90° kąt od horyzontu w górę) względem pozycji
obserwatora. Każda trajektoria satelity jest rysowana jako sekwencja
przedziałów czasowych odpowiadających rekordom nawigacyjnym
(efemerydom), z oznaczeniem PRN, strzałką kierunku oraz kolorami
wynikającymi z pola fitInterval w pliku RINEX.

Wejście (do zmodyfikowania bezpośrednio w kodzie programu)
-------
  - współrzędne ECEF pozycji obserwatora
  - nazwa pliku nawigacyjnego rinex
  - nazwa pliku obserwacyjnego rinex
  - tytuł wykresu

Wyjście
-------
  wykres nieba w pliku SVG navSkyplotForObsTimes.svg

Wydruk uruchomienia
-------------------
```
Total observation records:       651
Total observations:             7161
Number of observations
without attached ephemerides:      0
```

  ![sky plot example](navSkyplotForObsTimes.svg)