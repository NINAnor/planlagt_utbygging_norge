Overlappsanalyse med miljø- og samfunnsvariabler
==============================

*Beregnet for PostgreSQL med PostGIS utvidelsen*

**OVERLAPP.sql** inneholder en rekke SQL prosedyrer som kan lastes inn i en database.
Kjør skriptet opp mot database:
```
$ psql -d [database] -f OVERLAPP.sql
```

Kjør deretter ønsket prosedyre for å legge til areal-felt:
```
# call overlapp_myr();
# call overlapp_skog();
# call overlapp_jordbruk();
# call overlapp_aapen_fastmark();
...
```

-------
