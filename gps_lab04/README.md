2026-01-20

EN:
=====================================================================
  OBSERVATION TYPE PARSER FOR RINEX 3.04 FILE
=====================================================================

The program reads the observation types from the rinex file header.
It is assumed that the lines with observation types for a satellite
system cannot be separated by other lines, e.g. a comment.

Input
-----
  RINEX 3.04 observation file name

Output
------
  list of observation types grouped by satelite system
    
Print of run
------------
```
[('G',(8,["C1C","L1C","D1C","S1C","C2X","L2X","D2X","S2X"]))]
```

PL:
=====================================================================
  PARSER TYPÓW OBSERWACJI DLA PLIKU RINEX 3.04
=====================================================================

Program odczytuje typy obserwacji z nagłówka pliku rinex. Zakłada
się, że wiersze z typami obserwacji dla systemu satelitarnego nie mogą
być rozdzielone innymi wierszami, np. komentarzem.

Wejście
-------
  nazwa pliku obserwacyjnego RINEX 3.04

Wyście
------
  lista typów obserwacji pogrupowanych według systemu satelitarnego
    
Wydruk uruchomienia
-------------------
```
[('G',(8,["C1C","L1C","D1C","S1C","C2X","L2X","D2X","S2X"]))]
```
