
/*   Areal-summer   */
/* –––––––––––––––––––––––– */
/*
	Antakelser når prosedyrer i dette skriptet kjører

	Input tabeller:
	- Det finnes en tabell ved navn "planreserver" som inneholder aktuell geometri for overlappsanalysen
	- Det finnes følgende tabeller som inneholder aktuelle natur-/miljøområder:
		- "ar5";						NIBIOs AR5 data
		- "nve_100aar_skredfaresone";	NVEs datasett for 100års skredfaresoner
		- "nve_10aar_flomsone";			NVEs datasett for 10års flomsoner
		- "omraader_over_skoggrense";	Miljødirektoratets Natur i Norge (NiN) landskapstyper datasett med vegetasjontype lik 'Bart fjell over skoggrensen' eller 'Hei over skoggrensen'
		- "ssb_strandsone";				SSBs datasett for Potensielt tilgjengelig strandsone
		- "villrein_omraader";			Miljødirektoratets datasett for villreinsområder
		- "iba_norge_u_svalbard";		Datasett med viktige fugleområder i Norge (utenom Svalbard), fra birdlife.no
*/
/* –––––––––––––––––––––––– */

/* Areal prosent */
create or replace procedure area_percent()
language plpgsql
as $$
begin

	select
		kommunenummer,
		kommune,
		SUM(planlagt_utbygd_myr_m2)             / total_area.sum * 100 as "Planlagt utbygd myr",
		SUM(planlagt_utbygd_flomsone_m2)        / total_area.sum * 100 as "Planlagt utbygd flomsone",
		SUM(planlagt_utbygd_skog_m2)            / total_area.sum * 100 as "Planlagt utbygd skog",
		SUM(planlagt_utbygd_over_skoggrense_m2) / total_area.sum * 100 as "Planlagt utbygd over skoggrense",
		SUM(planlagt_utbygd_strandsone_m2)      / total_area.sum * 100 as "Planlagt utbygd strandsone",
		SUM(planlagt_utbygd_villrein_m2)        / total_area.sum * 100 as "Planlagt utbygd villrein",
		SUM(planlagt_utbygd_iba_m2)             / total_area.sum * 100 as "Planlagt utbygd IBA",
		SUM(planlagt_utbygd_jordbruk_m2)        / total_area.sum * 100 as "Planlagt utbygd jordbruk",
		SUM(planlagt_utbygd_aapen_fastmark_m2)  / total_area.sum * 100 as "Planlagt utbygd åpen fastmark",
		SUM(planlagt_utbygd_annen_arealtype_m2) / total_area.sum * 100 as "Planlagt utbygd annen arealtype",
		SUM(planlagt_utbygd_skredfaresone_m2)   / total_area.sum * 100 as "Planlagt utbygd skredfaresone"
	from
		"planreserver",
		(
			select SUM( ST_Area(geom) ) as sum
			from "planreserver"
		) as total_area
	group by
		kommunenummer,
		kommune,
		total_area.sum
	order by kommunenummer
	;

end; $$

/* Areal dekar */
create or replace procedure area_daa()
language plpgsql
as $$
begin

	select
		kommunenummer,
		kommune,
		SUM(planlagt_utbygd_myr_m2)             / 1000 as "Planlagt utbygd myr",
		SUM(planlagt_utbygd_flomsone_m2)        / 1000 as "Planlagt utbygd flomsone",
		SUM(planlagt_utbygd_skog_m2)            / 1000 as "Planlagt utbygd skog",
		SUM(planlagt_utbygd_over_skoggrense_m2) / 1000 as "Planlagt utbygd over skoggrense",
		SUM(planlagt_utbygd_strandsone_m2)      / 1000 as "Planlagt utbygd strandsone",
		SUM(planlagt_utbygd_villrein_m2)        / 1000 as "Planlagt utbygd villrein",
		SUM(planlagt_utbygd_iba_m2)             / 1000 as "Planlagt utbygd IBA",
		SUM(planlagt_utbygd_jordbruk_m2)        / 1000 as "Planlagt utbygd jordbruk",
		SUM(planlagt_utbygd_aapen_fastmark_m2)  / 1000 as "Planlagt utbygd åpen fastmark",
		SUM(planlagt_utbygd_annen_arealtype_m2) / 1000 as "Planlagt utbygd annen arealtype",
		SUM(planlagt_utbygd_skredfaresone_m2)   / 1000 as "Planlagt utbygd skredfaresone"
	from "planreserver"
	group by
		kommunenummer,
		kommune
	order by kommunenummer
	;

end; $$

/* –––––––––––––––––––––––– */







/*     Hjelpe prosedyrer    */
/* –––––––––––––––––––––––– */

/* Slett felt data */
create or replace procedure slett_felt(table_name text, field text)
language plpgsql
as $$
begin
	update table_name set field = null;
	commit;
end; $$

/* AR5, regne ut og fyll inn data */
create or replace procedure ar5_felt(table_name text, field text, arealtyper int[])
language plpgsql
as $$
begin

	select
		planreserver.ogc_fid,
		SUM( ST_Area( ST_Intersection( planreserver.geom, overlapp.geom ) ) ) as sum
	into temporary table temp_sums
	from
		table_name as planreserver,
		ar5 as overlapp
	where
		overlapp.arealtype = any ( arealtyper ) and
		ST_Intersects( planreserver.geom, overlapp.geom )
	group by planreserver.ogc_fid
	;

	update table_name as plan
	set field = res.sum
	from temp_sums as res
	where plan.ogc_fid = res.ogc_fid
	;

	commit;

end; $$

/* Område, regne ut og fyll inn data */
create or replace procedure omraade_felt(table_name text, field text, overlapp_name text)
language plpgsql
as $$
begin

	select
		planreserver.ogc_fid,
		SUM( ST_Area( ST_Intersection( planreserver.geom, overlapp.geom ) ) ) as sum
	into temporary table temp_sums
	from
		table_name as planreserver,
		overlapp_name as overlapp
	where
		ST_Intersects( planreserver.geom, overlapp.geom )
	group by planreserver.ogc_fid
	;

	update table_name as plan
	set field = res.sum
	from temp_sums as res
	where plan.ogc_fid = res.ogc_fid
	;

	commit;

end; $$

/* –––––––––––––––––––––––– */



/*    Overlapp    */
/* –––––––––––––––––––––––– */

/* MYR */
create or replace procedure overlapp_myr()
language plpgsql
as $$
begin

	call slett_felt('planreserver', 'planlagt_utbygd_myr_m2');
	call ar5_felt('planreserver', 'planlagt_utbygd_myr_m2', array[60]);
	commit;

end; $$



/* SKOG */
create or replace procedure overlapp_skog()
language plpgsql
as $$
begin

	call slett_felt('planreserver', 'planlagt_utbygd_skog_m2');
	call ar5_felt('planreserver', 'planlagt_utbygd_skog_m2', array[30]);
	commit;

end; $$



/* JORDBRUK */
create or replace procedure overlapp_jordbruk()
language plpgsql
as $$
begin

	call slett_felt('planreserver', 'planlagt_utbygd_jordbruk_m2');
	call ar5_felt('planreserver', 'planlagt_utbygd_jordbruk_m2', array[21, 22, 23]);
	commit;

end; $$



/* ÅPEN FASTMARK */
create or replace procedure overlapp_aapen_fastmark()
language plpgsql
as $$
begin

	call slett_felt('planreserver', 'planlagt_utbygd_aapen_fastmark_m2');
	call ar5_felt('planreserver', 'planlagt_utbygd_aapen_fastmark_m2', array[50]);
	commit;

end; $$



/* ANNEN AREALTYPE */
create or replace procedure overlapp_annen_arealtype()
language plpgsql
as $$
begin

	call slett_felt('planreserver', 'planlagt_utbygd_annen_arealtype_m2');
	call ar5_felt('planreserver', 'planlagt_utbygd_annen_arealtype_m2', array[81, 82, 70, 99]);
	commit;

end; $$



/* SKREDFARESONE */
create or replace procedure overlapp_skredfaresone()
language plpgsql
as $$
begin

	call slett_felt('planreserver', 'planlagt_utbygd_skredfaresone_m2');
	call omraade_felt('planreserver', 'planlagt_utbygd_skredfaresone_m2', 'nve_100aar_skredfaresone');
	commit;

end; $$



/* KANTSONE MOT VASSDRAG – 10-ÅR FLOMSONE */
create or replace procedure overlapp_flomsone()
language plpgsql
as $$
begin

	call slett_felt('planreserver', 'planlagt_utbygd_flomsone_m2');
	call omraade_felt('planreserver', 'planlagt_utbygd_flomsone_m2', 'nve_10aar_flomsone');
	commit;

end; $$



/* OMRÅDER OVER SKOGGRENSE */
create or replace procedure overlapp_over_skoggrense()
language plpgsql
as $$
begin

	call slett_felt('planreserver', 'planlagt_utbygd_over_skoggrense_m2');
	call omraade_felt('planreserver', 'planlagt_utbygd_over_skoggrense_m2', 'omraader_over_skoggrense');
	commit;

end; $$



/* STRANDSONE */
create or replace procedure overlapp_strandsone()
language plpgsql
as $$
begin

	call slett_felt('planreserver', 'planlagt_utbygd_strandsone_m2');
	call omraade_felt('planreserver', 'planlagt_utbygd_strandsone_m2', 'ssb_strandsone');
	commit;

end; $$



/* VILLREIN OMRÅDER */
create or replace procedure overlapp_villrein()
language plpgsql
as $$
begin

	call slett_felt('planreserver', 'planlagt_utbygd_villrein_m2');
	call omraade_felt('planreserver', 'planlagt_utbygd_villrein_m2', 'villrein_omraader');
	commit;

end; $$



/* IBA – IMPORTANT BIRD AREAS */
create or replace procedure overlapp_iba()
language plpgsql
as $$
begin

	call slett_felt('planreserver', 'planlagt_utbygd_iba_m2');
	call omraade_felt('planreserver', 'planlagt_utbygd_iba_m2', 'iba_norge_u_svalbard');
	commit;

end; $$

/* –––––––––––––––––––––––– */
