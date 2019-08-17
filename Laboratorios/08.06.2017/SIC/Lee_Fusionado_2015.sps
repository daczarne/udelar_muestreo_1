
GET
  FILE='G:\# Muestreo 2016\Lab ST SIC\Fusionado_2015_Terceros.sav'.

FILTER OFF.
USE ALL.
SELECT IF (dpto = 1).
EXECUTE.


SAVE OUTFILE='G:\# Muestreo 2016\Lab ST SIC\Ejemplo.sav'
  /DROP=anio e557 e558 e29_1 e29_2 e29_3 e29_4 e29_5 e29_5_1 e29_6 e30 e31 e32 e33 e34 e35 e36 e37 
    e37_2 e38 e38_1 e39 e39_2 e45_1 e45_1_1 e45_1_1_1 e45_1_2 e45_1_2_1 e45_2 e45_2_1 e45_2_1_1 e45_2_2 
    e45_2_2_1 e45_3 e45_3_1 e45_3_1_1 e45_3_2 e45_3_2_1 e45_4 e45_4_1 e45_4_2 e45_4_3 e45_4_3_1 e45_5 
    e45_5_1 e45_5_1_1 e45_6 e45_7 e45_7_1 e46 e47 e47_1 e48 e49 e51_10 e51_11 e51_2 e51_3 e51_4 e51_5 
    e51_6 e51_7 e51_7_1 e51_8 e51_9 e559 e559_1 e559_2 e560 e560_1 e560_1_1 e560_2 e560_2_1 e561 e561_1 
    e59 e60 e61 e62 e64_1 e64_2 e64_3 e64_4 e64_5 e64_6 e64_7 e65 e185 e186_1 e186_2 e186_3 e186_4 e193 
    e194 e196 e196_1 e196_2 e196_3 e197 e197_1 e198 e199 e200 e200_1 e200_2 e200_3 e201 e201_1 e202_1 
    e202_2 e202_3 e202_4 e202_5 e202_6 e202_6_1 e202_7 e202_8 e209_1 e210_1 e210_2 e210_3 e211 e211_1 
    e211_2 e211_3 e562 e212 e212_1 e213 e214_1 e215 e215_1 e216 e217_1 e218 e218_1 e219 e220_1 e221 
    e221_1 e222 e223_1 e224 e224_1 e225 e226_1 e234_2 e235_2 e236 estred13 nomdpto barrio nombarrio ccz 
    locagr nom_locagr e236_2 e236_4 e237 e238 e239 e240_1 e240_2 e241 e242 e242_1 e243_1 e243_2 e244 
    e245 e245_1 e246 e246_1 e247 f66 f67 f261 f68 f69 f70 f71_2 f72_2 f73 f74 f75 f76_2 f262 f263 f264 
    f264_1 f265 f265_1 f266 f267 f268 f77 f78 f79 f79_1 f80 f80_2 f81 f82 f83 f84 f85 f86 f87 f88_1 
    f88_2 f89 f90_2 f91_2 f92 f93 f94 f94_2 f95 f96 f97 f98 f99 f100 f101 f102 f103 f104 f105 f106 f107 
    f108 f109 f110 f111 f112 f113 f114 f115 f116 f117 f118_1 f118_2 f119_2 f120_2 f121 f122 f123 f124_1 
    f124_2 f124_3 f124_4 f124_5 f125 g126_1 g126_2 g126_3 g126_4 g126_5 g126_6 g126_7 g126_8 g127 
    g127_1 g127_2 g127_3 g128 g128_1 g129 g129_1 g129_2 g130 g130_1 g131 g131_1 g132 g132_1 g132_2 
    g132_3 g133 g133_1 g133_2 g134_1 g134_2 g134_3 g134_4 g134_5 g134_6 g134_7 g134_8 g135 g135_1 
    g135_2 g135_3 g136 g136_1 g137 g137_1 g137_2 g138 g138_1 g139 g139_1 g140 g140_1 g140_2 g140_3 g141 
    g141_1 g141_2 g142 g143 g144 g144_1 g144_2_1 g144_2_2 g144_2_3 g144_2_4 g144_2_5 g145 g146 g147 
    g148_1_1 g148_1_2 g148_1_3 g148_1_4 g148_1_5 g148_1_6 g148_1_7 g148_1_8 g148_1_9 g148_1_10 
    g148_1_11 g148_1_12 g148_2_1 g148_2_2 g148_2_3 g148_2_4 g148_2_5 g148_2_6 g148_2_7 g148_2_8 
    g148_2_9 g148_2_10 g148_2_11 g148_2_12 g148_3 g148_4 g148_5_1 g148_5_2 g149 g149_1 g150 g151_1 
    g151_2 g151_3 g151_3_1 g151_4 g151_5 g152 g153 g153_1 g153_2 g154 g154_1 g250_1 g250_2 g250_3 
    g250_4 g251_1 g251_2 g251_3 g251_4 g255 g256 g257 g258 g258_1 indabajo indaceliac indadiabet 
    indaoncolo indaplomo indarenal indarendia indasida indatuberc mto_almue mto_caball mto_cuota 
    mto_desay mto_emer mto_hogcon mto_oveja mto_vacas pt1 pt2 pt4 subempleo lecheenpol c1 c2 c3 c4 c5_1 
    c5_2 c5_3 c5_4 c5_5 c5_6 c5_7 c5_8 c5_9 c5_10 c5_11 c5_12 c6 d7 d8_1 d8_2 d8_3 d8_4 d9 d10 d11 d12 
    d13 d14 d15 d16 d18 d260 d19 d20 d21_1 d21_2 d21_3 d21_4 d21_4_1 d21_5 d21_5_1 d21_6 d21_7 d21_7_1 
    d21_7_2 d21_7_3 d21_8 d21_9 d21_10 d21_11 d21_12 d21_13 d21_14 d21_14_1 d21_15 d21_15_1 d21_15_2 
    d21_15_3 d21_15_4 d21_16_1 d21_16_2 d21_16_3 d21_17 d21_18 d21_18_1 d21_19 d21_19_1 d23 d24 d25 
    d181 d184 d184_1 d229 d230 d231 d232 h155 h155_1 h156 h156_1 h158_1 h158_2 h159 h160 h160_1 h160_2 
    h161 h162 h163_1 h163_2 h164 h165 h166 h269 h269_1 h167_1 h167_2 h167_3 h167_4 h167_1_1 h167_1_2 
    h167_2_1 h167_2_2 h167_3_1 h167_3_2 h167_4_1 h167_4_2 h169 h170_1 h170_2 h271 h271_1 h171 h171_1 
    h171_2 h172 h172_1 h173 h173_1 h227 h252 h252_1 i174 i175 i228 i259 ht1 ht2 ht3 ht4 ht5 ht6 ht7 ht8 
    ht9 ht10 ht11 ht13 ht14 ht19 mes dpto YSVL pesomen pesotri pesosem li_06 lp_06 indigente06 pobre06 
    region_4 region_3
  /COMPRESSED.
