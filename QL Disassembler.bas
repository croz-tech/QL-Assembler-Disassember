100 WINDOW 448,200,32,16
110 OPEN#2;scr_:CLOSE#2
120 MODE 4
130 CSIZE 3,1
140 PRINT "68000 Disassembler"
150 CSIZE 2,0
160 PRINT "®1984 Alan Giles" :REMark ** replace with QL copyright **
170 CSIZE 0,0
180 REPeat display
190 PRINT "Code or Data? (C or D)";
200 REPeat inmode
210 a$=INKEY$(-1)
220 IF a$=="c" OR a$=="d" THEN EXIT inmode
230 END REPeat inmode
240 PRINT !a$
250 REPeat instart
260 INPUT "Start address? (Use $ to indicate hexadecimal)"!b$
270 start=number (b$)
280 IF start>=0 THEN EXIT instart
290 END REPeat instart
300 REPeat infin
310 INPUT "Finish address? (Use ENTER to carry on forever)"!c$
320 IF c$="" THEN c$="$FFFFF"
330 finish=number(c$)
340 IF finish>=0 THEN EXIT infin
350 END REPeat infin
360 IF a$=="c" THEN
370 disassemble start, finish
380 ELSE
390 SCROLL -10
400 CURSOR 0,190
410 FOR i=start TO finish STEP 32
420 PRINT hex5$(i)!;
430 FOR j=i TO i+31
440 PRINT hexcon$(j);
450 END FOR j
460 SCROLL -10
470 CURSOR 39,190
480 FOR j=i TO i+31
490 IF PEEK(j)<>10 THEN
500 PRINT CHR$(PEEK(j))!;
510 ELSE
520 PRINT "\;";
530 END IF
540 END FOR j
550 PRINT
560 IF INKEY$=="s" THEN NEXT display
570 END FOR i
580 END IF
590 END REPeat display

600 DEFine FuNction number (a$)
610 LOCal i,j,k
620 IF a$="" THEN RETurn -1
630 IF a$(1)>="0" AND a$(1)<="9" THEN RETurn a$
640 IF a$(1)<>"$" THEN RETurn -1
650 i=0
660 FOR j=2 TO LEN(a$)
670 k=CODE(a$(j))
680 IF k>=CODE("a") THEN k=k-32
690 SELect ON k
700 =CODE("0") TO CODE("9"): i=i*16+k-CODE("0")
710 =CODE("A") TO CODE("F"): i=i*16+k-55
720 =REMAINDER: RETurn -1
730 END SELect
740 END FOR j
750 RETurn i
760 END DEFine

770 DEFine FuNction hex5$(a)
780 LOCal i,a$
790 a$=""
800 FOR i=4 TO 0 STEP -1
810 a$=a$&hex$(INT(a/16^i)-16*INT(a/16^(i+1)))
820 END FOR i
830 RETURN a$
840 END DEFine

850 DEFine FuNction hexcon$(a)
860 RETURN hex$(PEEK(a) DIV 16)&hex$(PEEK(a)MOD 16)
870 END DEFine

880 DEFine FuNction hex$(a)
890 IF a<10 THEN RETURN a
900 RETURN CHR$(55+a)
910 END DEFine

920 DEFine PROCedure disassemble(start,finish)
930 LOCal i,pc,as
940 IF start/2<>INT(start/2) THEN start=start-1
950 REPeat loop
960 IF INKEY$=="s" THEN RETurn
970 IF start>finish THEN PRINT FILL$(" ", 29); "END":RETurn
980 PRINT hex5$(start)!;
990 pc=start
1000 fault=0
1010 a$=dis$(pc)
1020 IF fault THEN
1030 pc=start+2
1040 a$="DC.B    "
1050 IF PEEK(start)>=CODE(" ") AND PEEK(start)<=CODE("®") THEN :REMark ** replace with QL copyright **
1060 a$=a$&""&CHR$(PEEK(start))&"","
1070 ELSE
1080 a$=a$&PEEK(start)&","
1090 END IF
1100 IF PEEK(start+1)>=CODE(" ") AND PEEK(start+1)<=CODE("®") THEN :REMark ** replace with QL copyright **
1110 a$=a$&"'"&CHR$(PEEK(start+1))&"'"
1120 ELSE
1130 a$=a$&PEEK(start+1)
1140 END IF
1150 END IF
1160 FOR i=start TO start+10
1170 IF i>=pc THEN
1180 PRINT "  ";
1190 ELSE
1200 PRINT hexcon$(i);
1210 END IF
1220 END FOR i
1230 PRINT !a$
1240 start=pc
1250 END REPeat loop
1260 END DEFine

1270 DEFine FuNction dis$(pc)
1280 LOCal j
1290 j=PEEK (pc) DIV 16+1
1300 ON j GO TO 1310,1320,1330,1340,1350,1360,1370,1380,1390,1400,1410,1420,1430,1440,1450,1460
1310 RETurn dis0$(pc)
1320 RETurn dis1$(pc)
1330 RETurn dis2$(pc)
1340 RETurn dis3$(pc)
1350 RETurn dis4$(pc)
1360 RETurn dis5$(pc)
1370 RETurn dis6$(pc)
1380 RETurn dis7$(pc)
1390 RETurn dis8$(pc)
1400 RETurn dis9$(pc)
1410 fault=1: RETurn ""
1420 RETurn disB$(pc)
1430 RETurn disC$(pc)
1440 RETurn disD$(pc)
1450 RETurn disE$(pc)
1460 fault=1:RETurn """
1470 END DEFine

1480 DEFine FuNction dis1$(pc)
1490 LOCal i,j,as
1500 a$="MOVE.B  "
1510 i=PEEK (pc)*4 MOD 64+PEEK(pc+1) DIV 64
1520 IF i MOD 8=1 THEN fault=1:RETurn ""
1530 IF i MOD 8=7 AND i>15 THEN fault=1:RETurn ""
1540 IF PEEK(pc+1) MOD 64=60 THEN
1550 pc=pc+4
1560 IF PEEK (pc-2)<>0 AND PEEK(pc-2)<>255 THEN fault=1: RETurn ""
1570 a$=a$&"#$"&hexcon$(pc-1)
1580 ELSE
1590 j=PEEK(pc+1) MOD 64
1600 IF j DIV 8=1 THEN fault=1:RETurn ""
1610 pc=pc+2
1620 a$=a$&adr$(j DIV 8,j MOD 8,pc)
1630 END IF
1640 RETurn a$&","&adr$(i MOD 8,i DIV 8,pc)
1650 END DEFine

1660 DEFine FuNction dis2$(pc)
1670 LOCal i,j,a$
1680 a$="MOVE.L  "
1690 i=PEEK(pc)*4 MOD 64+PEEK(pc+1) DIV 64
1700 IF i MOD 8=7 AND i>15 THEN fault=1:RETurn ""
1710 IF PEEK(pc+1) MOD 64=60 THEN
1720 pc=pc+6
1730 a$=a$&"#$"&hexcon$(pc-4)&hexcon$(pc-3)&hexcon$(pc-2)&hexcon$(pc-1)
1740 ELSE
1750 j=PEEK(pc+1) MOD 64
1760 pc=pc+2
1770 a$=a$&adr$(j DIV 8,j MOD 8,pc)
1780 END IF
1790 RETurn a$&","&adr$(i MOD 8,i DIV 8,pc)
1800 END DEFine

1810 DEFine FuNction dis3$(pc)
1820 LOCal i,j,a$
1830 a$="MOVE.W  "
1840 i=PEEK(pc)*4 MOD 64+PEEK(pc+1) DIV 64
1850 IF i MOD 8=7 AND i>15 THEN fault=1:RETurn ""
1860 IF PEEK(pc+1) MOD 64=60 THEN
1870 pc=pc+4
1880 a$=a$&"#$"&hexcon$(pc-2)&hexcon$(pc-1)
1890 ELSE
1900 j=PEEK(pc+1) MOD 64
1910 pc=pc+2
1920 a$=a$&adr$(j DIV 8,j MOD 8,pc)
1930 END IF
1940 RETurn a$&","&adr$(i MOD 8,i DIV 8,pc)
1950 END DEFine : REMark ** AI added this **
1960 DEFine FuNction adr$(m,j,pc)
1970 LOCal k,a$
1980 type=m
1990 reg=j
2000 SELect ON type
2010 =0:RETurn "D"&reg
2020 =1:RETurn "A"&reg
2030 =2:RETurn "(A"&reg&")"
2040 =3:RETurn "(A"&reg&")+"
2050 =4:RETurn "-(A"&reg&")"
2060 =5:pc=pc+2:RETurn "$"&hexcon$(pc-2)&hexcon$(pc-1)&" (A"&reg&")"
2070 =6:k=PEEK(pc):pc=pc+2
2080 a$="$"&hexcon$(pc-1)&" (A"&reg&","
2090 RETurn a$&index$(k)
2100 =7:SELect ON reg
2110 =0:pc=pc+2:RETurn "$"&hexcon$(pc-2)&hexcon$(pc-1)
2120 =1:pc=pc+4:RETurn "$"&hexcon$(pc-4)&hexcon$(pc-3)&hexcon$(pc-2) &hexcon$(pc-1)
2130 =2:pc=pc+2: RETurn "$"&hexcon$(pc-2)&hexcon$(pc-1)&"(PC)=$"&hex5$(pc-2+256*PEEK(pc-2)-PEEK(pc-2) DIV 128*65536+PEEK(pc-1))
2140 =3:k=PEEK(pc):pc=pc+2
2150 a$="$"&hexcon$(pc-1)&" (PC,"
2160 RETurn a$&index$(k)
2170 =REMAINDER: fault=1: RETurn ""
2180 END SELect
2190 END SELect
2200 END DEFine

2210 DEFine FuNction index$(k)
2220 LOCal a$
2230 IF k<128 THEN
2240 a$="D"
2250 ELSE
2260 a$="A"
2270 END IF
2280 a$=a$&(k MOD 128 DIV 16)
2290 IF k MOD 8<>0 THEN fault=1:RETurn ""
2300 IF k MOD 16=0 THEN
2310 RETurn a$&".W)"
2320 END IF
2330 RETurn a$&".L)"
2340 END DEFine

2350 DEFine FuNction dis0$(pc)
2360 LOCal i,a$
2370 i=PEEK (pc)
2380 IF I MOD 2=0 AND I<>8 THEN
2390 IF I DIV 4 MOD 2=1 THEN IF PEEK (pc+1) MOD 64=60 THEN fault=1:RETurn ""
2400 a$=imm$(pc): IF fault THEN RETurn ""
2410 SELect ON i
2420 0:RETurn "OR"&a$(1 TO 2)&" "&a$(3 TO)
2430 2:RETurn "AND"&a$
2440 4:RETurn "SUB"&a$
2450 6:RETurn "ADD"&a$
2460 10:RETurn "EOR"&a$
2470 12:RETurn "CMP"&a$
2480 REMAINDER: fault=1:RETurn ""
2490 END SELect
2500 END IF
2510 IF PEEK(pc+1) DIV 8 MOD 8=1 THEN
2520 IF I=8 THEN fault=1:RETurn ""
2530 a$="MOVEP"
2540 IF PEEK (pc+1) DIV 64 MOD 2=0 THEN
2550 a$=a$&".W "
2560 ELSE
2570 a$=a$&".L "
2580 END IF
2590 pc=pc+4
2600 IF PEEK(pc-3) DIV 128=0 THEN
2610 RETurn a$&"$"&hexcon$(pc-2)&hexcon$(pc-1)&"(A" & (PEEK(pc-3) MOD 8)&"), D"& (PEEK(pc-4) DIV 2)
2620 END IF
2630 RETurn a$&"D" & (PEEK (pc-4) DIV 2)&", $"&hexcon$ (pc-2) &hexcon$(pc-1)&"(A" & (PEEK(pc-3) MOD 8)&")"
2640 END IF
2650 i=PEEK(pc+1) DIV 64
2660 IF PEEK (pc+1) MOD 64>=58 THEN fault=1:RETurn ""
2670 SELect ON i
2680 0:a$="BTST"
2690 1:a$="BCHG"
2700 2:a$="BCLR"
2710 3:a$="BSET"
2720 END SELect
2730 i=PEEK(pc+1) MOD 64
2740 a$=a$&"    "
2750 IF i DIV 8=1 THEN fault=1:RETurn ""
2760 IF PEEK(pc)=8 THEN
2770 IF PEEK(pc+2)<>0 THEN fault=1:RETurn ""
2780 a$=a$&"#"&PEEK (pc+3)
2790 pc=pc+4
2800 ELSE
2810 a$=a$&"D" & (PEEK (pc) DIV 2)
2820 pc=pc+2
2830 END IF
2840 RETurn a$&","&adr$(i DIV 8,i MOD 8,pc)
2850 END DEFine

2860 DEFine FuNction imm$(pc)
2870 LOCal i,j,as
2880 i=PEEK (pc+1) MOD 64
2890 j=PEEK (pc+1) DIV 64
2900 SELECT ON j
2910 0:a$=".B   #$"&hexcon$(pc+3):pc=pc+4
2920 IF PEEK (pc-2)<>0 AND PEEK (pc-2) <>255 THEN fault=1: RETurn ""
2930 1:a$=".W   #$"&hexcon$(pc+2)&hexcon$(pc+3)
2940 pc=pc+4
2950 2:a$=".L   #$"&hexcon$(pc+2)&hexcon$(pc+3)&hexcon$(pc+4)&hexcon$(pc+5)
2960 pc=pc+6
2970 3:fault=1:RETurn ""
2980 END SELect
2990 IF i=60 THEN RETurn a$&",SR"
3000 IF i DIV 8=1 OR i>=58 THEN fault=1:RETurn ""
3010 RETurn a$&","&adr$(i DIV 8,i MOD 8,pc)
3020 END DEFine

3030 DEFine FuNction dis4$(pc)
3040 LOCal i,j,k,a,b$
3050 IF PEEK(pc)MOD 2=1 THEN
3060 i=PEEK (pc) DIV 2 MOD 8
3070 IF PEEK(pc+1)<192 THEN
3080 a$="CHK     "
3090 IF PEEK (pc+1)=188 THEN
3100 pc=pc+4
3110 RETurn a$&"#"&hexcon$(pc-2)&hexcon$(pc-1)&", D"&i
3120 END IF
3130 pc=pc+2
3140 j=PEEK(pc-1) MOD 64
3150 IF PEEK(pc-1)<128 OR j DIV 8=1 THEN fault=1:RETurn ""
3160 RETurn a$&adr$(j DIV 8,j MOD 8,pc)&", D"&i
3170 END IF
3180 pc=pc+2
3190 j=PEEK(pc-1) MOD 64
3200 IF j<=15 OR (j>=24 AND j<=39) THEN fault=1: RETurn ""
3210 RETurn "LEA"&adr$(j DIV 8,j MOD 8,pc)&", A"&i
3220 END IF
3230 i=PEEK(pc) MOD 16
3240 pc=pc+2
3250 j=PEEK (pc-1) DIV 64
3260 k=PEEK (pc-1) MOD 64
3270 IF i<14 AND k DIV 8=1 THEN fault=1:RETurn ""
3280 SELect ON i
3290 =0: SELect ON j
3300 =0:a$="NEGX.B"
3310 =1:a$="NEGX.W"
3320 =2:a$="NEGX.L"
3330 =3:a$="MOVESR,"
3340 END SELect
3350 IF k>=58 THEN fault=1:RETurn ""
3360 RETurn a$&adr$(k DIV 8,k MOD 8,pc)
3370 =2:SELect ON j
3380 =0:a$="CLR.B"
3390 =1:a$="CLR.W"
3400 =2:a$="CLR.L"
3410 =3:fault=1:RETurn ""
3420 END SELect
3430 IF k>=58 THEN fault=1:RETurn ""
3440 RETurn a$&""&adr$(k DIV 8,k MOD 8,pc)
3450 =4:SELect ON j
3460 =3: IF k=60 THEN
3470 IF PEEK(pc)<>0 THEN fault=1: RETurn ""
3480 pc=pc+2
3490 a$="#$"&hexcon$(pc-1)
3500 ELSE
3510 a$=adr$(k DIV 8,k MOD 8,pc)
3520 END IF
3530 RETurn "MOVEAAAA"&$&", CCR"
35400:a$="NEG. B"
35501:a$="NEG.W"
3560 2:a$="NEG.L"
3570 END SELect
3580 IF k>=58 THEN fault=1:RETurn ""
3590 RETurn a$&"AAA"&adr$ (k DIV B,k MOD 8,pc)
36006: SELect ON j
36103: IF k=60 THEN
3620 a$="#$"&hexcon$ (pc) &hexcon$(pc+1)
3630 pc=pc+2
3640 ELSE
3650 a$=adr$(k DIV 8,k MOD B,pc)
3660 END IF
3670 RETurn "MOVE" &$&", SR"
36800: a$="NOT.B"
3690 1:a$="NOT.W"
3700 2:a$="NOT.L"
3710 END SELect
3720 IF k>=58 THEN fault=1:RETurn ""
3730 RETurn a$&"AAA"&adr$ (k DIV 8,k MOD 8,pc)
3740 8: SELect ON j
37500:IF k>=5B THEN fault=1:RETurn ""
3760 RETurn "NBCDA"&adr$(k DIV 8,k MOD 8,pc)
37701:IF k<B THEN RETurn "SWAPAAAAD &K
3780 IF k=15 OR (k)=24 AND k<=39) THEN fault=1:RETurn ""
3790 RETurn "PEA" &adr$ (k DIV 8,k MOD 8,pc)
3800 2: SELect ON K
38108 TO 7:RETurn "EXT. WAAAD"&k
3820 32 TO 39:RETurn "MOVEM.WA."&regmaskpredec$(pc)&",-(A*&(k MOD 8)
3830 16 TO 23,40 TO 57: RETurn "MOVEM.W."&regmaskpostinc$(pc)&","&ad r$(k DIV B,k MOD 8,pc)
3840 REMAINDER: fault=1:RETurn ""
3850 END SELect
3860 3: SELect ON K
38700 TO 7:RETurn "EXT. LAAAD"&k
3880 32 TO 39:RETurn "MOVEM.L."&regmaskpredec$(pc)&",-(A"&(k MOD 8) &")"
3890 16 TO 23,40 TO 57:RETurn "MOVEM.L."&regmaskpostinc$(pc)&","&ad r$(k DIV 8,k MOD 8,pc)
3900 REMAINDER :fault=1:RETurn ""
3910 END SELect
3920 END SELect
3930 10: SELect ON j
3940 0:a$="TST.B"
3950 1:a$="TST.W"
3960 2:a$="TST.L"
3970 3:a$="TAS"
3980 END SELect
3990 IF k>=58 THEN fault=1:RETurn ""
4000 RETurn a$&""&adr$(k DIV 8,k MOD 8,pc)
4010 12: IF j<2 OR k<16 OR K DIV 8>4 THEN fault=1:RETurn""
4020 a$=regmaskpostinc$(pc)
4030 IF j=2 THEN
4040 b$="W"
4050 ELSE
4060 b$="L"
4070 END IF
4080 RETurn "MOVEM."&b$&adr$(k DIV 8,k MOD 8,pc)&", "&a$
4090 14:RETurn dis4E$(pc,j,k)
4100 END SELect
4110 END DEFine
4120 DEFine FuNction regmaskpostinc$(pc)
4130 LOCal a$, d$,i,j
4140 a$=""
4150 j=PEEK(pc)
4160 FOR i=0 TO 7
4170 IF j DIV 2^i MOD 2=1 THEN a$=a$&"A"&i
4180 END FOR i
4190 compress a$
4200 d$=""
4210 pc=pc+2
4220 j=PEEK (pc-1)
4230 FOR i=0 TO 7
4240 IF j DIV 2^i MOD 2=1 THEN d$=d$&"D"&i
4250 END FOR i
4260 compress d$
4270 RETurn combine$(d$,a$)
4280 END DEFine
4290 DEFine FuNction regmaskpredec$(pc)
4300 LOCal a$,d$,i,j
4310 d$=""
4320 j=PEEK (pc)
4330 FOR i=0 TO 7
4340 IF j*2^i DIV 128 MOD 2=1 THEN d$=d$&"D"&i
4350 END FOR i
4360 compress d$
4370 a$=""
4380 pc=pc+2
4390 j=PEEK (pc-1)
4400 FOR i=0 TO 7
4410 IF j*2^i DIV 128 MOD 2=1 THEN a$=a$&"A"&i
4420 END FOR i
4430 compress a$
4440 RETurn combine$(d$,a$)
4450 END DEFine
4460 DEFine PROCedure compress (a$)
4470 LOCal i,j
4480 i=LEN(a$)
4490 SELect ON i
4500 =0 TO 2: RETurn
4510 =4:a$=a$(1 TO 2)&"/"&a$(3 TO 4):RETurn
4520 REMAINDER
4530 FOR j=2 TO i-4 STEP 2
4540 IF a$(j)+1=a$(j+2) THEN
4550 IF a$(j)+2=a$(j+4) THEN a$(j+1)="-"
4560 END IF
4570 END FOR j
4580 j=3
4590 REPeat juggle
4600 IF j>LEN(a$) THEN RETurn
4610 IF a$(j)<>"-" THEN a$=a$(1 TO j-1)&"/"&a$(j TO):j=j+3: NEXT juggle
4620 IF a$(j+2)="-" THEN a$=a$(1 TO j-1)&a$(j+2 TO): NEXT juggle
4630 a$=a$(1 TO j)&a$(j+2 TO)
4640 j=j+3
4650 END REPeat juggle
4660 END SELect
4670 END DEFine
4680 DEFine FuNction combine$(d$,a$)
4690 IF d$="" THEN
4700 IF a$="" THEN fault=1:RETurn ""
4710 RETurn a$
4720 END IF
4730 IF a$="" THEN RETurn d$
4740 RETurn d$&"/"&a$
4750 END DEFine
4760 DEFine FuNction dis4E$(pc,j,k)
4770 IF j=0 THEN fault=1: RETurn ""
4780 IF j>1 THEN
4790 IF k<=15 OR (k>=24 AND k<=39) THEN fault=1:RETurn ""
4800 IF j=2 THEN RETurn "JSRAAAAA"&adr$(k DIV 8,k MOD 8,pc)
4810 RETurn "JMPAAAAA"&adr$(k DIV 8,k MOD 8,pc)
4820 END IF
4830 SELect ON k
4840 0 TO 15: RETurn "TRAPAAAA#$"&hex$(k)
4850 16 TO 23:pc=pc+2
4860 RETurn "LINKA"&(K MOD 8)&",#$"&hexcon$(pc-2)&hexcon$(pc-1)
4870 24 TO 31: RETurn "UNLKAA"&(K MOD 8)
4880 32 TO 39: RETurn "MOVEAAAAA"&(k MOD 8)&", USP"
4890 40 TO 47: RETurn "MOVE USP, A"&(K MOD 8)
4900 48:RETurn "RESET"
4910 49: RETurn "NOP"
4920 50:pc=pc+2
4930 RETurn "STOP&#$"&hexcon$(pc-2)&hexcon$(pc-1)
4940 51: RETurn "RTE"
4950 53: RETurn "RTS"
4960 54: RETurn "TRAPV"
4970 55: RETurn "RTR"
4980 REMAINDER : fault=1:RETurn ""
4990 END SELect
5000 END DEFine
5010 DEFine FuNction dis7$(pc)
5020 IF PEEK(pc)MOD 2=1 THEN fault=1: RETurn ""
5030 pc=pc+2
5040 RETurn "MOVER#$"&hexcon$(pc-1)&", D" & (PEEK (pc-2) DIV 2 MOD 8)
5050 END DEFine
5060 DEFine FuNction dis5$(pc)
5070 LOCal i,j,as
5080 i=PEEK (pc)MOD 16
5090 j=PEEK(pc+1)
5100 pc=pc+2
5110 IF j<192 THEN
5120 IF i MOD 2-0 THEN
5130 a$="ADDO."
5140 ELSE
5150 a$="SUBQ."
5160 END IF
5170 IF MOD 64>=58 THEN fault=1:RETurn ""
5180 IF 12 THEN i=16
5190 SELect ON j
5200 8 TO 15:fault=1:RETurn ""
5210 0 TO 63:a$=a$&"B"
5220 64 TO 127:a$=a$&"W"
5230 REMAINDER: a$=a$&"L"
5240 END SELect
5250 RETurn a$&#"&(i DIV 2)&","&adr$(j DIV 8 MOD 8,j MOD 8,pc)
5260 END IF
5270 IF j DIV 8 MOD 8=1 THEN
5280 IF j>=250 THEN fault=1:RETurn ""
5290 RETurn "S"&con$(i)&""&adr$(j DIV 8 MOD 8,j MOD 8,pc)
5300 END IF
5310 pc=pc+2
5320 RETurn "DB" &con$(i)&"D"&(j MOD 8)&", $"&hexcon$(pc-2)&hexcon$(pc-1)&" (PC)=$"&hex5$(pc-2+256*PEEK (pc-2)-PEEK (pc-2) DIV 128*65536+PEEK(pc-1))
5330 END DEFine
5340 DEFine FuNction dis6$(pc)
5350 LOCal i,j,as
5360 i=PEEK(pc)MOD 16
5370 j=PEEK(pc+1)
5380 pc=pc+2
5390 a$=con$(i)
5400 IF a$(2)="" THEN.
5410 IF a$="TA" THEN
5420 a$="RA"
5430 ELSE
5440 a$="SR"
5450 END IF
5460 END IF
5470 IF j<>0 THEN RETurn "B"&a$&".SA$"&hexcon$(pc-1)&" (PC)=$"&hex5$(pc+j-j DIV 128*256)
5480 pc=pc+2
5490 RETurn "B"&a$&"$"&hexcon$(pc-2)&hexcon$(pc-1)&" (PC)=$"&hex5$(pc-2+256*PEEK(pc-2)-PEEK(pc-2) DIV 128*65536+PEEK(pc-1))
5500 END DEFine
5510 DEFine FuNction con$(i)
5520 SELect ON i
5530 0:RETurn "Ta"
5540 1:RETurn "FA"
5550 2:RETurn "HI"
5560 3:RETurn "LS"
5570 4:RETurn "CC"
5580 5:RETurn "CS"
5590 6:RETurn "NE"
5600 7:RETurn "EQ"
5610 8:RETurn "VC"
5620 9:RETurn "VS"
5630 10:RETurn "PL"
5640 11:RETurn "MI"
5650 12:RETurn "GE"
5660 13:RETurn "LT"
5670 14:RETurn "GT"
5680 15:RETurn "LE"
5690 END SELect
5700 END DEFine
5710 DEFine FuNction dis9$(pc):RETurn "SUB"&dis9orD$(pc):END DEFine
5720 DEFine FuNction disD$(pc):RETurn "ADD"&dis9orD$(pc):END DEFine
5730 DEFine FuNction dis9orD$(pc)
5740 LOCal i,j,k,as
5750 i=PEEK(pc)MOD 16
5760 j=PEEK (pc+1) DIV 64
5770 k=PEEK (pc+1) MOD 64
5780 pc=pc+2
5790 IF i MOD 2=0 THEN
5800 SELect ON J
5810 3: IF k=60 THEN
5820 pc=pc+2
5830 RETurn ".W#$"&hexcon$(pc-2) &hexcon$ (pc-1)&", A"&(i DIV 2)
5840 END IF
5850 RETurn ".WAAA"&adr$(k DIV 8,k MOD 8,pc)&", A"&(i DIV 2)
5860 0:a$=".B"
5870 1:a$=".W"
5880 2:a$=".L"
5890 END SELect
5900 RETurn a$&""&adr$(k DIV 8,k MOD 8,pc)&", D"&(i DIV 2)
5910 END IF
5920 SELect ON j
5930 3: IF k=60 THEN
5940 pc=pc+4
5950 RETurn ".Laax#$"&hexcon$(pc-4)&hexcon$(pc-3)&hexcon$(pc-2) &hexc
on$(pc-1)&", A" & (i DIV 2)
5960 END IF
5970 RETurn "LA" &adr$(k DIV 8,k MOD 8,pc)&", A"&(i DIV 2)
5980 0:a$=".B"
5990 1:a$=".W"
6000 2:a$=".L"
6010 END SELect
6020 IF k<8 THEN
6030 RETurn "X"&a$&"D" &k&", D"&(i DIV 2)
6040 END IF
6050 IF k<16 THEN
6060 RETurn "X"&a$&"-(A&(k-8)&"),-(A&li DIV 2)&")"
6070 END IF
6080 IF k>=58 THEN fault=1:RETurn ""
6090 RETurn a$&"D" & (i DIV 2)&", "&adr$(k DIV 8,k MOD 8,pc)
6100 END DEFine
6110 DEFine FuNction dis8$(pc)
6120 LOCal i,j,k,a$
6130 i=PEEK(pc)MOD 16
6140 j=PEEK(pc+1) DIV 64
6150 k=PEEK(pc+1)MOD 64
6160 pc=pc+2
6170 IF i MOD 2=0 THEN
6180 IF k DIV 8=1 THEN fault=1:RETurn ""
6190 SELect ON j
6200 3:IF k=60 THEN
6210 pc=pc+2
6220 RETurn "DIVU####"&hexcon$(pc-2)&hexcon$(pc-1)&", D"&(i DIV 2)
6230 END IF
6240 RETurn "DIVU####"&adr$(k DIV 8,k MOD 8,pc)&", D"&(i DIV 2)
6250 0:a$="OR.B"
6260 1:a$="OR.W"
6270 2:a$="OR.L"
6280 END SELect
6290 RETurn a$&""&adr$(k DIV 8,k MOD 8,pc)&", D" & (i DIV 2)
6300 END IF
6310 SELect ON j
6320 3:IF k=60 THEN
6330 pc=pc+2
6340 RETurn "DIVS####"&hexcon$(pc-2)&hexcon$(pc-1)&", D" & (i DIV 2)
6350 END IF
6360 IF k DIV 8=1 THEN fault=1:RETurn
6370 RETurn "DIVS####"&adr$(k DIV 8,k MOD 8,pc)&", D"&(i DIV 2)
6380 0:IF k<8 THEN
6390 RETurn "SBCDAD" &k&", D" & (i DIV 2)
6400 END IF
6410 IF k<16 THEN
6420 RETurn "SBCD####(A&(k-8)&"),-(A"&(i DIV 2)&")"
6430 END IF
6440 a$="DR.B"
6450 1:a$="OR.W"
6460 2:a$="DR.L"
6470 END SELect
6480 IF k<16 OR k>=58 THEN fault=1:RETurn ""
6490 RETurn a$&" AD" & (i DIV 2)&", "&adr$(k DIV 8,k MOD 8,pc)
6500 END DEFine
6510 DEFine FuNction disB$(pc)
6520 LOCal i,j,k,a$
6530 i=PEEK (pc)MOD 16
6540 j=PEEK(pc+1)DIV 64
6550 k=PEEK (pc+1) MOD 64
6560 pc=pc+2
6570 IF i MOD 2=0 THEN
6580 SELECT ON j
6590 3: IF k=60 THEN
6600 pc=pc+2
6610 RETurn "CMP.WAAA#$"&hexcon$(pc-2)&hexcon$(pc-1)&", A"&(i DIV 2)
6620 END IF
6630 RETurn "CMP.WAAA"&adr$( k DIV 8,k MOD 8,pc)&", A" & (i DIV 2)
6640 0: IF K DIV 8=1 THEN fault=1:RETurn **
6650 a$="CMP.B"
6660 1:a$="CMP.N"
6670 2:a$="CMP.L"
6680 END SELect
6690 RETurn a$&""&adr$(k DIV 8,k MOD 8,pc)&", D"&(i DIV 2)
6700 END IF
6710 SELECT ON j
6720 3: IF k=60 THEN
6730 pc=pc+4
6740 RETurn "CMP.LA#$"&hexcon$(pc-4)&hexcon$(pc-3)&hexcon$(pc-2)&h excon$(pc-1)&", A"&(i DIV 2)
6750 END IF
6760 RETurn "CMP.LAAA"&adr$(k DIV 8,k MOD 8,pc)&", A"&(i DIV 2)
6770 0:a$="B"
6780 1:a$="W"
6790 =2:a$="L"
6800 END SELect
6810 IF k>=58 THEN fault=1:RETurn ""
6820 IF K DIV 8=1 THEN RETurn "CMPM. "&a$&" (A"&(k-8)&")+, (A"&(i DIV 2)&")+"
6830 RETurn "EOR. "&a$&"D" & (i DIV 2)&", "&adr$(k DIV 8,k MOD 8,pc)
6840 END DEFine
6850 DEFine FuNction disC$(pc)
6860 LOCal i,j,k,at
6870 i=PEEK (pc) MOD 16
6880 j=PEEK(pc+1) DIV 64
6890 k=PEEK (pc+1) MOD 64
6900 pc=pc+2
6910 IF I MOD 2=0 THEN
6920 IF k DIV 8=1 THEN fault=1:RETurn "".
6930 SELect ON j
6940 3: IF k=60 THEN
6950 pc=pc+2
6960 RETurn "MULU####"&hexcon$(pc-2)&hexcon$(pc-1)&", D" & (i DIV 2)
6970 END IF
6980 RETurn "MULU####"&adr$(k DIV 8,k MOD 8,pc)&", D"&(i DIV 2)
6990 0: a$="AND.B"
7000 1:a$="AND.W"
7010 2:a$="AND.L"
7020 END SELect
7030 RETurn a$&""&adr$(k DIV 8,k MOD 8,pc)&", D1 DIV 2)
7040 END IF
7050 SELect ON j
7060 0:IF k<8 THEN RETurn "ABCD####D" &k&", D"&(i DIV 2)
7070 IF k<16 THEN RETurn "ABCD####(A*&(k-8)&"),-(A&(i DIV 2)&")"
7080 a$-AND.B"
7090 1:IF k<8 THEN RETurn "EXB####AD" & (i DIV 2)&", D"&k
7100 IF k<16 THEN RETurn "EXG####A" & (i DIV 2)&", A"&(k-8)
7110 a$="AND.W"
7120 2:IF k<8 THEN fault=1: RETurn ""
7130 IF k<16 THEN RETurn "EXG####AD" & (i DIV 2)&", A"&(k-8)
7140 a$="AND.L"
7150 3: IF k=60 THEN
7160 pc=pc+2
7170 RETurn "MULS####"&hexcon$(pc-2)&hexcon$(pc-1)&", D"&(i DIV 2)
7180 END IF
7190 IF K DIV 8=1 THEN fault=1:RETurn ""
7200 RETurn "MULS####"&adr$(k DIV 8,k MOD 8,pc)&", D*&(i DIV 2)
7210 END SELect
7220 IF k>=58 THEN fault=1:RETurn ""
7230 RETurn a$&"D" & (i DIV 2)&", "&adr$(k DIV 8,k MOD 8,pc)
7240 END DEFine
7250 DEFine FuNction disE$(pc)
7260 LOCal i,j,k,as
7270 i=PEEK (pc) MOD 16
7280 j=PEEK (pc+1)DIV 64
7290 k=PEEK (pc+1) MOD 64
7300 pc=pc+2
7310 SELect ON j
7320 3: IF k<16 OR k>=58 THEN fault=1:RETurn "".
7330 RETurn shift$(".", i) &adr$(k DIV 8,k MOD 8,pc)
7340 =0:a$=".B"
7350 =1:a$=".W"
7360 =2:a$=".L"
7370 END SELect
7380 j=k DIV 8 MOD 4*2+i MOD 2
7390 IF k<32 THEN
7400 IF i<2 THEN i=16
7410 RETurn shift$(a$,j)&"#"&(i DIV 2)&", D"&(k MOD 8)
7420 END IF
7430 RETurn shift$(a$,j)&"D"&(i DIV 2)&", D"&(k MOD 8)
7440 END DEFine
7450 DEFine FuNction shift$(a$,n)
7460 SELect ON n
7470 =0:RETurn "ASR"&a$&""
7480 =1:RETurn "ASL"&a$&""
7490 =2:RETurn "LSR"&a$&""
7500 =3:RETurn "LSL"&a$&""
7510 =4:RETurn "ROXR"&a$&""
7520 =5:RETurn "ROXL"&a$&""
7530 =6:RETurn "ROR"&a$&""
7540 =7:RETurn "ROL"&a$&""
7550 REMAINDER :fault=1:RETurn ""
7560 END SELect
7570 END DEFine
7580 REMark End of Disassembler.