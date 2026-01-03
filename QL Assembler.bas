100 WINDOW 448,200,32,16
110 OPEN #2;scr_:CLOSE #2
120 MODE 4
130 prog$=""
140 CSIZE #0;2,0
150 topline=0:tiline=-1
160 codespace=0:codesize=0
170 codeaddr=0
180 REPEAT control
190 CLS
200 CSIZE 3,1
210 AT 1,1;PRINT "68000 Assembler"
220 CSIZE 1,0
230 PRINT ©1984,Alan,Giles,-Version"!ver$
240 CSIZE 2,0
250 PRINT \\ "Current program size":LEN(prog$)/60!"lines"(1 TO 5-(LEN(prog$)/60=ii)
260 PRINT \ "Code start":codeaddr
270 PRINT \ "Space reserved":codespace!"bytes"
280 PRINT \ "Space used":codesize!"bytes"
290 spare=RESPR(0)-241000-2*LEN(prog$):IF spare<0 THEN spare=0
300 PRINT \ "Spare space":spare!"bytes"
310 CLS#0
320 PRINT #0;"A=Assemble""E=Edit""L=Load code";
330 IF codesize>0 THEN PRINT#0;"R=Run code""S=Save code";
340 key=CODE(INKEY$(1))
350 IF key=CODE("Z") THEN key=key-32
360 SELECT ON key
370 =CODE("A"):assemble
380 =CODE("E"):editor
390 =CODE("L")
400 CLS#0
410 IF codesize>0 THEN
420 PRINT#0;"Use existing code space?(y/n)"
430 a$=INKEY$(-1)
440 ELSE
450 a$="N"
460 END IF
470 IF a$=="N" THEN
480 CLS#0
490 INPUT#0;"Bytes needed?"!a$
500 codesize="0"&a$
510 IF codesize=0 THEN NEXT control
520 codespace codesize
530 codeaddr=RESPR(codespace)
540 END IF
550 CLS#0
560 INPUT#0;"Drive number"!d$:d="0"&d$: IF d=0 THEN codesize=codesize(NOT(a$=="N")): NEXT control
570 INPUT#0;"File name"!name$
580 LBYTES "mdv"&d&"_"&name$,codeaddr
590 codesize=codespace
600 CODE("R")
610 IF codesize 0 THEN NEXT control
620 CLS#0
630 INPUT#0;"Start address"!d$:d="0"&d$: IF d=0 THEN d=codeaddr
640 usr codeaddr
650 CODE("S")
660 IF codesize=0 THEN NEXT control
670 CLS#0
680 INPUT#0;"Drive number"!d$:d="0"&d$: IF d=0 THEN NEXT control
690 INPUT#0;"File name"!name$
700 DELETE "mdv"&d&"_"&name$
710 SBYTES "mdv"&d&"_"&name$,codeaddr, codesize
720 END SELect
730 END REPeat control
740 DEFine PROCedure editor
750 CSIZE 1,0
760 CLS
770 autolist
780 REPeat operation
790 CLS#0
800 PRINT#0;"A=Append"! "D=Delete"! "I=Insert"! "L=Load"! "M=Merge";
810 IF LEN(prog$)/60>topline+19 THEN PRINT #0;! "N=Next page";
820 IF topline>0 THEN PRINT #0;! "P=Previous page";
830 PRINT #0;! "R=Replace"! "S=Save"!"X=eXit"
840 key=CODE(INKEY$(-1))
850 IF key>CODE("Z") THEN key=key-32
860 SELect ON key
870 CODE("A"):iline=-1:insert
880 CODE("D"): del "Delete":autolist
890 CODE("I")
900 CLS#0
910 INPUT#0;\"Insert before line"!ilines: IF iline$<>"" THEN iline ="0"&iline$
920 IF iline>=LEN (prog$)/60 THEN iline=-1
930 insert
940 CODE("L")
950 prog$="":iline=-1:mergefile
960 CODE("M")
970 CLS#0
980 INPUT#0;\"Merge before line"!ilines: IF iline$<>"" THEN iline= "0"&iline$
990 IF iline>=LEN (prog$)/60 THEN iline=-1
1000 mergefile
1010 CODE("N")
1020 topline=topline+20
1030 autolist
1040 CODE("P")
1050 topline=topline-20
1060 autolist
1070 CODE("R"): del "Replace":insert
1080 CODE("S")
1090 CLS#0
1100 INPUT#0;\"Drive number"!d$\"File name"!name$:d="0"&d$: IF d=0 THEN d=1
1110 DELETE "mdv"&d&" "&name$
1120 OPEN_NEW#4; "mdv"&d&"_"&name$
1130 FOR i=1 TO LEN(prog$) STEP 60
1140 PRINT #4;prog$(i TO i+59)
1150 END FOR i
1160 CLOSE#4
1170 CODE("X"):RETurn
1180 END SELect
1190 END REPeat operation
1200 END DEFine
1210 DEFine PROCedure autolist
1220 AT 0,0
1230 IF topline>LEN(prog$)/60-19 THEN topline=LEN(prog$)/60-19
1240 IF topline<0 THEN topline=0
1250 FOR i=topline TO topline+19
1260 p=i*60+22
1270 1$="000"&i: PRINT "";i$(LEN(1$)-3 TO);
1280 IF p>LEN(prog$) THEN EXIT i
1290 IF i=iline THEN PRINT "A";CHR$(189);
1300 IF prog$(p)="*" THEN PRINT,prog$(p TO p+38):NEXT i:RETurn
1310 PRINT,prog$(p TO p+4),prog$(p+5 TO p+11),prog$(p+12 TO p+38)
1320 NEXT i
1330 RETurn
1340 END FOR i
1350 IF iline=-1 THEN PRINT "A";CHR$(189);
1360 PRINT,,"END";:CLS 4:CLS 2
1370 END DEFine
1380 DEFine PROCedure insert
1390 REPeat ins
1400 autolist
1410 IF 2*LEN(prog$)>RESPR(0)-241000 THEN PRINT "Program space full":RETurn
1420 getline
1430 IF line$="" THEN RETurn
1440 SELect ON iline
1450 -1:prog$=prog$&line$
1460 =0:prog$=line$&prog$:iline=1
1470 =REMAINDER
1480 prog$=prog$(1 TO iline*60)&line$&prog$(iline*60+1 TO)
1490 iline=iline+1
1500 END SELect
1510 topline=iline-10
1520 IF iline=-1 THEN topline=600
1530 END REPeat ins
1540 END DEFine
1550 DEFine PROCedure mergefile
1560 CLS#0
1570 INPUT#0;\"Drive number"!d$\"File name"!name$:d="0"&d$: IF d=0 THEN d=1
1580 OPEN_IN#4; "mdv"&d&"_"&name$
1590 REPeat mergeline
1600 IF EOF (#4) THEN EXIT mergeline
1610 IF 2*LEN(prog$)>=RESPR()-241000 THEN autolist: PRINT \"Program space full: CLOSE#4: RETurn
1620 INPUT#4; line$
1630 SELect ON iline
1640 -1:prog$=prog$&line$
1650 0:prog$=line$&prog$:iline=1
1660 REMAINDER
1670 prog$=prog$(1 TO iline*60)&line$&prog$(iline*60+1 TO)
1680 iline=iline+1
1690 END SELect
1700 END REPeat mergeline
1710 CLOSE#4
1720 topline-iline-2
1730 IF iline=-1 THEN topline=600
1740 autolist
1750 END DEFine
1760 DEFine PROCedure del (as)
1770 CLS#0
1780 INPUT#0;\(a$)!"from"!iline$;: IF iline$<>"" THEN iline="0"&iline $
1790 IF iline>=LEN(prog$)/60 THEN iline -1: RETurn
1800 topline=iline-10
1810 INPUT#0;! "to"! deline$:deline="0"&delines: IF deline= THEN delin e=iline
1820 IF deline iline THEN RETurn
1830 IF deline>=LEN(prog$)/60-1 THEN
1840 prog$=prog$(1 TO iline 60)
1850 iline=-1
1860 RETurn
1870 END IF
1880 IF iline=0 THEN
1890 prog$=prog$(deline*60+61 TO)
1900 RETurn
1910 END IF
1920 prog$=prog$(1 TO iline*60)&prog$(deline*60+61 TO)
1930 END DEFine
1940 DEFine PROCedure getline
1950 line$=FILL$(".",60)
1960 eror$=""
1970 value=0:lastd=0:dsize=0
1980 REPeat getlabel
1990 CLS#2
2000 INPUT#0;\"Label "\in$
2010 IF in$<>"" THEN
2020 IF in$(1)="+" THEN
2030 IF LEN(in$)>39 THEN NEXT getlabel
2040 line$(22 TO 60)=in$
2050 line$(1 TO 2)="00"
2060 RETurn
2070 END IF
2080 END IF
2090 IF LEN(in$)>5 THEN NEXT getlabel
2100 line$(22 TO 26)=in$
2110 FOR i=1 TO LEN(in$)
2120 IF in$(i)<"0" OR (in$(i)>"9" AND in$(i)<"A") OR in$(i)>"z" THE
N NEXT getlabel
2130 END FOR i
2140 EXIT getlabel
2150 END REPeat getlabel
2160 REPeat getcommand
2170 CLS#0
2180 INPUT#0;(eror$)\,"Command"\(line$(22 TO 26)),in$
2190 IF in$="" THEN line$="":RETurn
2200 eror$="Invalid parameter"
2210 IF LEN(in$)>7 THEN NEXT getcommand
2220 line$(27 TO 33)=in$
2230 initial=CODE(in)
2240 IF initial>CODE("Z") THEN initial=initial-32
2250 SELect ON initial
2260 CODE("A"):comA
2270 CODE("B"):comB
2280 CODE("C"):comC
2290 CODE("D"):comD
2300 CODE("E"):comE
2310 CODE("J"):comJ
2320 CODE("L"):comL
2330 CODE("M"):comM
2340 CODE("N"):comN
2350 CODE("O"):comO
2360 CODE("P"):comP
2370 CODE("R"):comR
2380 CODE("S"):comS
2390 CODE("T"):comT
2400 CODE("U"):comU
2410 REMAINDER: eror$="Invalid mnemonic"
2420 END SELect
2430 IF line$(1)<>"" THEN RETurn
2440 line$(1 TO 21)=""
2450 END REPeat getcommand
2460 END DEFine
2470 DEFine PROCedure comA
2480 IF line$(28 TO 33)=="BCDAAA" THEN ext "C", "0": RETurn
2490 IF line$(28 TO 29)=="DD" THEN addorsub: RETurn
2500 IF line$(28 TO 29)=="ND" THEN logical: RETurn
2510 IF line$(28 TO 29)=="SL" OR line$(28 TO 29)=="SR" THEN shift: RETurn
2520 eror$="Invalid mnemonic"
2530 END DEFine
2540 DEFine PROCedure ext (d1$,d3$)
2550 size=0
2560 get 2params: IF line$(1)="" THEN RETurn
2570 IF type<>type2 THEN line$(1)="": RETurn
2580 IF type=0 THEN
2590 line$(2 TO 5)=d1$&hex$(reg2*2+1)&d3$&reg
2600 RETurn
2610 END IF
2620 IF type=4 THEN
2630 line$(2 TO 5)=d1$&hex$(reg2*2+1)&d3$&hex$(reg+8)
2640 RETurn
2650 END IF
2660 lines(1)=""
2670 END DEFine
2680 DEFine FuNction hex$(a)
2690 IF a<10 THEN RETurn a
2700 RETurn CHR$(a+55)
2710 END DEFine
2720 DEFine PROCedure get2params
2730 getparam
2740 value1=16*INT(value/16)+lastd
2750 dsize1=dsize
2760 IF line$(1)="" THEN RETurn
2770 IF excess$="" THEN
2780 getsecond
2790 ELSE
2800 second$=excess$
2810 END IF
2820 identify second$, type2, reg2
2830 IF excess$<>"" OR (type2=8 AND NOT(line$(27 TO 33)=="LINKAAA") ) OR (type2=7 AND reg2>1 AND NOT(line$(27 TO 28)=="DB")) THEN line$(1)=""
2840 END DEFine
2850 DEFine PROCedure getparam
2860 line$(1)="1"
2870 excess$=""
2880 CLS#0
2890 INPUT#0;\,, "Parameter"\(line$(22 TO 26)), (line$(27 TO 33)),in$
2900 IF in$="" OR LEN(in$)>27 THEN line$(1)="": RETurn
2910 line$(34 TO 60)=in$
2920 identify (in$), type, reg
2930 END DEFine
2940 DEFine PROCedure getsecond
2950 CLS#0
2960 INPUT#0;\,,, "Next"\(line$(22 TO 26)), (line$(27 TO 33)), (in$);", "; second$
2970 in$=in$&", "&second$
2980 IF second$="" OR LEN(in$)>27 THEN line$(1)="": RETurn
2990 line$(34 TO 60)=in$
3000 END DEFine
3010 DEFine PROCedure identify(source$, type, reg)
3020 type=-1:reg=-1:excess$=""
3030 IF source$="" THEN line$(1)="": RETurn
3040 IF source$(1)=="D" THEN type=0:procreg:RETurn
3050 IF source$(1)=="A" THEN type=1:procreg:RETurn
3060 IF source$(1)=="$" OR (source$(1)>="0" AND source$(1)<="9") THEN
procnumber:RETurn
3070 IF source$(1)=="(" THEN
3080 type=2
3090 IF LEN (source$)<4 THEN line$(1)="":RETurn
3100 IF source$(2 TO 3)=="SP" THEN source$(2 TO 3)="A7"
3110 IF LEN(source$)>=5 THEN IF source$(5)="+" THEN type=3
3120 IF source$(2)=="A" THEN
3130 IF source$(3)<"0" OR source$(3)>"7" OR source$(4)<>">" THEN line$(1)="":RETurn
3140 reg=source$(3)
3150 IF LEN(source$)>2+type THEN
3160 IF source$(3+type)<>"," THEN line$(1)="":RETurn
3170 IF LEN(source$)>3+type THEN excess$=source$(4+type TO)
3180 END IF
3190 RETurn
3200 END IF
3210 line$(1)="":RETurn
3220 END IF
3230 IF source$(1)=="-" THEN
3240 IF source$(2)<>"(" THEN procnumber:RETurn
3250 type=4
3260 IF LEN(source$)<5 THEN line$(1)="":RETurn
3270 IF source$(3 TO 4)=="SP" THEN source$(3 TO 4)="A7"
3280 IF source$(3)=="A" THEN
3290 IF source$(4)<"0" OR source$(4)>"7" OR source$(5)<>")" THEN line$(1)="":RETurn
3300 reg=source$(4)
3310 IF LEN(source$)>5 THEN
3320 IF source$(6)<>"," THEN line$(1)="":RETurn
3330 IF LEN(source$)>6 THEN excess$=source$(7 TO)
3340 END IF
3350 RETurn
3360 END IF
3370 line$(1)="":RETurn
3380 END IF
3390 IF source$(1)="#" THEN
3400 type=8
3410 IF LEN(source$)=1 THEN line$(1)="": RETurn
3420 source$=source$(2 TO)
3430 procnumber:RETurn
3440 END IF
3450 IF source$(1)="" OR source$(1)='"' THEN.procnumber:RETurn
3460 proclabel
3470 END DEFine
3480 DEFine PROCedure procreg
3490 IF LEN(source$)<2 THEN proclabel:RETurn
3500 IF source$(2)<"0" OR source$(2)>"7" THEN proclabel:RETurn
3510 reg=source$(2)
3520 IF LEN(source$)>2 THEN
3530 IF source$(3)<>"," THEN proclabel:RETurn
3540 IF LEN(source$)>3 THEN excess$=source$(4 TO)
3550 END IF
3560 END DEFine
3570 DEFine PROCedure procnumber
3580 IF source$(1)="$" THEN
3590 extracthex
3600 ELSE
3610 IF source$(1)="" OR source$(1)='"' THEN
3620 extracttext
3630 ELSE
3640 extractdecimal
3650 END IF
3660 END IF
3670 IF LEN(source$)>0 THEN
3680 IF source$(1)<>"," THEN
3690 IF source$(1)<>"(" THEN line$(1)="":RETurn
3700 procindex:RETurn
3710 END IF
3720 IF LEN(source$)>1 THEN excess$=source$(2 TO)
3730 END IF
3740 IF type=-1 THEN
3750 IF value<32768 THEN
3760 type=7:reg=0:datasize=1
3770 ELSE
3780   type=7:reg=1:datasize=2
3790 END IF
3800 END IF
3810 IF type=B THEN datasize=size
3820 deposit value,datasize
3830 END DEFine
3840 DEFine PROCedure extracthex
3850 value=0:lastd=0
3860 FOR i=2 TO LEN(source$)
3870   source$=source$(2 TO)
3880   digit=CODE(source$)
3890   IF digit>CODE("Z") THEN digit=digit-32
3900   SELect ON digit
3910     CODE("0") TO CODE("9"): value=value*16+source$(1):lastd=source$(1)
3920     CODE("A") TO CODE("F"): value=value*16+digit-55:lastd=digit-55
3930     CODE("-"): IF i=2 THEN 3940
3940       source$="-"&source$(3 TO): extractdecimal: RETurn
3950     ELSE
3960       line$(1)="": RETurn
3970     END IF
3980     REMAINDER: RETurn
3990   END SELect
4000 END FOR i
4010 source$=""
4020 END DEFine
4030 DEFine PROCedure extractdecimal
4040   sign=1
4050   IF source$(1)="-" THEN sign=-1:source=source$(2 TO): IF source$(1)="$" THEN extracthex: value=-value:lastd=16-lastd:RETurn
4060   IF source$(1)="" OR source$(1)='"' THEN extracttext:value=-value:lastd=16-lastd: RETurn
4070   value=0:lastd=0
4080   FOR i=1 TO LEN(source$)
4090   IF source$(i)<"0" OR source$(i)>"9" THEN RETurn
4100   value=value*10+sign*source$(i): lastd=(lastd*10+sign*source$(i)) MOD 16
4110   IF LEN(source$)=i THEN source$="": RETurn
4120 source$=source$(2 TO)
4130 END FOR i
4140 END DEFine
4150 DEFine PROCedure extracttext
4160 IF type<>8 THEN line$(1)="": RETurn
4170 codetext
4180 IF value>256^(2^size) THEN line$(1)="": RETurn
4190 REPeat left
4200 IF value <256^(2^size-1) THEN value=value*256:lastd=0: NEXT left
4210 EXIT left
4220 END REPeat left
4230 END DEFine
4240 DEFine PROCedure codetext
4250 quote$=source$(1)
4260 value=0:lastd=0
4270 IF LEN(source$)<2 THEN line$(1)="": RETurn
4280 FOR i=2 TO LEN(source$)
4290 source$=source$(2 TO)
4300 IF source$(1)=quote$ THEN
4310 IF LEN(source$)=1 THEN source$="": RETurn
4320 IF source$(2)="," THEN source$=source$(2 TO): RETurn
4330 IF source$(2)=quote$ THEN value=value*256+CODE(quote$): lastd=CODE(quote$) MOD 16:i=i+1:source$=source$(2 TO): NEXT i
4340 line$(1)="": RETurn
4350 ELSE
4360 value=value*256+CODE(source$): lastd=CODE(source$) MOD 16
4370 END IF
4380 END FOR i
4390 line$(1)=""
4400 END DEFine
4410 DEFine PROCedure prociabel
4420 type=7:reg=2
4430 length=LEN(source$)
4440 IF length>5 THEN length=5
4450 FOR i=1 TO length
4460 IF source$(i)<"0" OR (source$(i)>"9" AND source$(i)<"A") OR source$(i)>"Z" THEN EXIT i
4470 NEXT i
4480 i=length+1
4490 END FOR i
4500 IF i=1 THEN line$(1)="": RETurn
4510 label$=source$(1 TO i-1)
4520 excess$=""
4530 IF i<=LEN(source$) THEN
4540 IF source$(i)="," THEN
4550 IF i<LEN(source) THEN excess$=source$(i+1 TO)
4560 ELSE
4570 IF source$(i)<>"(" THEN line$(1)="": RETurn
4580 procpcindex: RETurn
4590 END IF
4600 END IF
4610 IF label$="SR" THEN type=9:reg=1: RETurn
4620 IF label$="CCR" THEN type=9:reg=0:RETurn
4630 IF label$="SP" OR label$="USP" THEN type=1:reg=7:RETurn
4640 IF line$(1)<>"1" AND line$(27 TO 31)<>"MOVEM" AND line$(27 TO 31)<>"moven" THEN line$(1)="": RETurn
4650 line$(2+4*line$(1) TO 5+4*line$(1))="xxxx"
4660 line$(1)=line$(1)+1
4670 END DEFine
4680 DEFine PROCedure procindex
4690 IF LEN(source$)<4 THEN line$(1)="": RETurn
4700 IF source$(2 TO 3)="SP" THEN source$(2 TO 3)="A7"
4710 IF source$(2 TO 4)="USP" OR source$(2 TO 4)="SSP" THEN source$="(A7"&source$(5 TO)
4720 IF source$(2 TO 3)="PC" THEN type=7:source$(2 TO 3)="A3"
4730 IF (source$(1 TO 2)<>"(A" AND source$(1 TO 2)<>"(a") OR source$(3)<"0" OR source$(3)>"7" THEN line$(1)="": RETurn
4740 reg=source$(3)
4750 IF LEN(source$)>4 THEN
4760 IF source$(4)="," THEN
4770 IF value>255 OR value<-128 THEN line$(1)="": RETurn
4780 IF type<>7 THEN type=6
4790 byte=-1
4800 IF source$(5 TO 6)="SP" THEN source$(5 TO 6)="A7"
4810 IF source$(5)="D" THEN byte=0
4820 IF source$(5)="A" THEN byte=128
4830 IF byte<0 OR LEN(source$)<7 THEN line$(1)="": RETurn
4840 IF source$(6)<"0" OR source$(6)>"7" THEN line$(1)="": RETurn
4850 byte=byte+16*source$(6)
4860 IF source$(7)=")" THEN
4870 IF LEN(source$)>7 THEN excess$=source$(8 TO)
4880 ELSE
4890 IF source$(7)<>"." OR LEN(source$) <9 THEN line$(1)="": RETurn
4900 IF source$(9)<>")" THEN line$(1)="": RETurn
4910 IF LEN(source$)9 THEN excess$=source$(10 TO)
4920 IF source$(8)=="L" THEN
4930 byte=byte+8
4940 ELSE
4950 IF NOT (source$(8)=="W") THEN line$(1)="": RETurn
4960 END IF
4970 END IF
4980 IF excess$<>"" THEN
4990 IF excess$(1)<>"," THEN line$(1)="": RETurn
5000 IF LEN(excess$)>1 THEN excess$=excess$(2 TO)
5010 END IF
5020 IF value <0 THEN value=value+256
5030 value=value+256*byte
5040 deposit value,1
5050 RETurn
5060 END IF
5070 END IF
5080 IF source$(4)<>")" THEN line$(1)="": RETurn
5090 IF type<>7 THEN type=5
5100 IF type=7 THEN reg=2
5110 excess$=""
5120 IF LEN(source$)>4 THEN
5130 IF source$(5)<>"," THEN line$(1)="": RETurn
5140 IF LEN(source)>5 THEN excess$=source$(6 TO)
5150 END IF
5160 deposit value,1
5170 END DEFine
5180 DEFine PROCedure deposit(value, size)
5190 IF size=2 THEN
5200 IF lastd<4 THEN value=value+4
5210 IF lastd>=12 THEN value=value-4
5220 END IF
5230 IF value <0 THEN value=value+256^(2^size)
5240 IF value<0 OR value>=256^(2^size) THEN line$(1)="": RETurn
5250 dsize=size
5260 IF size=0 THEN dsize=1
5270 pointer=4*(line$(1)+dsize)+1
5280 line$(1)=line$(1)+dsize
5290 FOR i=0 TO 4*dsize-1
5300 lines(pointer-i)=hex$(INT (value/16^i-16*INT(value/16^(i+1))))
5310 END FOR i
5320 IF size=2 THEN lines(pointer)=hex$(lastd)
5330 END DEFine
5340 DEFine PROCedure procpcindex
5350 source=source$(i TO)
5360 type=7:reg=3
5370 IF line$(1)<>"1" AND NOT(line$(27 TO 31)=="MOVEM") THEN line$(1)="":RETurn
5380 line$(1)=line$(1)+1
5390 line$(line$(1)*4-2 TO line$(1)*4+1)="xxxx"
5400 IF source$(2 TO 3)=="PC" THEN
5410 IF source$(4)=")" THEN
5420 excess$=""
5430 IF LEN(source$) >4 THEN
5440 IF source$(5)<>"," THEN line$(1)="": RETurn
5450 IF LEN(source$) >5 THEN excess$=source$(6 TO)
5460 END IF
5470 reg=2: RETurn
5480 END IF
5490 IF source$(4)<>"," OR LEN(source$)<5 THEN line$(1)="": RETurn
5500 source="("&source$(5 TO)
5510 END IF
5520 IF LEN(source$)<4 THEN line$(1)="": RETurn
5530 IF source$(2 TO 3)=="SP" THEN source$(2 TO 3)="A7"
5540 byte=-1
5550 IF source$(2)=="D" THEN byte=0
5560 IF source$(2)=="A" THEN byte=128
5570 IF byte<0 OR source$(3)<"0" OR source$(3)>"7" THEN line$(1)="":RETurn
5580 byte=byte+source$(3)*16
5590 IF source$(4)=")" THEN
5600 IF LEN(source$)>4 THEN excess$=source$(5 TO)
5610 ELSE
5620 IF source$(4)<>"." OR LEN(source$)<6 THEN line$(1)="": RETurn
5630 IF source$(6)<>")" THEN line$(1)="": RETurn
5640 IF LEN(source$)>6 THEN excess$=source$(7 TO)
5650 IF source$(5)=="L" THEN
5660   byte=byte+8
5670 ELSE
5680 IF NOT(source$(5)=="W") THEN line$(1)="": RETurn
5690 END IF
5700 END IF
5710 IF excess$<>"" THEN
5720 IF excess$(1)<>"," OR LEN(excess$)<2 THEN line$(1)="": RETurn
5730   excess$=excess$(2 TO)
5740 END IF
5750 line$(4*line$(1)-2 TO 4*line$(1)-1)=hex$(byte DIV 16)&hex$(byte
MOD 16)
5760 END DEFine

5770 DEFine PROCedure getsize(n)
5780 fault=0
5790 IF n<33 THEN IF line$(n+1 TO 33)<>FILL$(",", 33-n) THEN fault=1: RETurn
5800 IF line$(n)=="B" THEN size=0: RETurn
5810 IF line$(n)=="W" THEN size=1: RETurn
5820 IF line$(n)=="L" THEN size=2: RETurn
5830 fault=1
5840 END DEFine

5850 DEFine PROCedure addorsub
5860 size=1
5870 di$="D"
5880 IF line$(27)=="S" THEN di$="9"
5890 IF line$(30)=="X" THEN
5900 IF line$(31)="," THEN
5910   getsize 32: IF fault THEN RETurn
5920 ELSE
5930 IF line$(31 TO 33)<>"" THEN RETurn
5940 END IF
5950 ext di$,4*size
5960 RETurn
5970 END IF
5980 IF line$(30)="." THEN getsize 31: IF fault THEN RETurn
5990 IF line$(31)="." THEN getsize 32: IF fault THEN RETurn
6000 get2params: IF line$(1)="" THEN RETurn
6010 IF (line$(30)=="0" OR line$(30)=="1") AND type<>8 THEN line$(1)="" : RETurn
6020 IF line$(30)=="A" AND type2<>1 THEN line$(1)="": RETurn
6030 IF type=B AND NOT(line$(30)=="1") AND value1<9 AND value1>0 THEN
6040 IF type2>7 THEN line$(1)="": RETurn
6050 d2$="0"
6060 IF line$(27)=="5" THEN d2$="1"
6070 d2$=hex$(value1 MOD 8*2+d2$)
6080 line$(6 TO 13)=line$(dsize1*4+6 TO dsize1*4+13)
6090 line$(14 TO 21)=""
6100 line$(1)=line$(1)-dsize1
6110 line$(2 TO 5)="5"&d2$&hex$(size*4+type2 DIV 2)&hex$(type2 MOD 2*8+reg2)
6120 RETurn
6130 END IF
6140 IF type2=1 THEN
6150 IF type=8 THEN type=7:reg=4
6160 line$(2 TO 5)=d1$&hex$(reg2+2+size-1)&hex$(12+type DIV 2)&hex$(type MOD 2*8+reg)
6170 RETurn
6180 END IF
6190 IF type=8 THEN
6200 d1$="96"
6210 IF line$(27)=="S" THEN d1$="04"
6220 line$(2 TO 5)=d1$&hex$(size*4+type2 DIV 2)&hex$(type2 MOD 2*8+reg2)
6230 RETurn
6240 END IF
6250 IF type2=0 THEN
6260 line$(2 TO 5)=d1$&hex$(reg2+2)&hex$(size*4+type DIV 2)&hex$(type MOD 2*8+reg)
6270 RETurn
6280 END IF
6290 IF type=0 THEN
6300 line$(2 TO 5)=d1$&hex$(reg*2+1)&hex$(size*4+type2 DIV 2)&hex$(type2 MOD 2*8+reg2)
6310 RETurn
6320 END IF
6330 line(1)=""
6340 END DEFine

6350 DEFine PROCedure logical
6360 size=1
6370 IF line$(29)="." THEN getsize 30: IF fault THEN RETurn
6380 IF line$(30)="." THEN getsize 31: IF fault THEN RETurn
6390 IF line$(31)="," THEN getsize 32: IF fault THEN RETurn
6400 get2params: IF line$(1)="" THEN RETurn
6410 IF (line$(29)=="1" OR line$(30)=="I") AND type<>8 THEN line$(1) ="":RETurn
6420 IF type=8 THEN
6430 IF line$(27)=="A" THEN d1$="02"
6440 IF line$(27)=="E" THEN d1$="0A"
6450 IF line$(27)=="0" THEN d1$="00"
6460 IF type2=9 THEN type2=7:reg2=4: IF size=2 THEN line$(1)="": RETurn
6470 line$(2 TO 5)=d1$&hex$(size*4+type2 DIV 2)&hex$(type2 MOD 2*8+ reg2)
6480 RETurn
6490 END IF
6500 IF line$(27)=="A" THEN d1$="C"
6510 IF line$(27)=="E" THEN d1$="B"
6520 IF line$(27)=="0" THEN d1$="8"
6530 IF type2=0 AND d1$<>"B" THEN
6540 IF type=1 OR type=7 THEN line$(1)="": RETurn
6550 line$(2 TO 5)=d1$&hex$(reg2+2)&hex$(size*4+type DIV 2)&hex$(type MOD 2*8+reg)
6560 RETurn
6570 END IF
6580 IF type<>0 OR type2=1 OR type2>7 THEN line$(1)="": RETurn
6590 line$(2 TO 5)=d1$&hex$(reg*2+1)&hex$(size*4+type2 DIV 2)&hex$(type2 MOD 2*8+reg2)
6600 END DEFine

6610 DEFine PROCedure shift
6620 size=1
6630 IF line$(30)="." THEN getsize 31: IF fault THEN RETurn
6640 IF line$(31)="." THEN getsize 32: IF fault THEN RETurn
6650 d2$="0"
6660 IF line$(27)=="L" THEN d2$="2"
6670 IF line$(27)=="R" THEN d2$="6"
6680 IF line$(29)=="L" THEN d2$=d2$+1
6690 IF line$(29)=="X" THEN
6700 IF d2$<>"6" THEN RETurn
6710 d2$="4"
6720 IF line$(30)=="L" THEN d2$="5"
6730 END IF
6740 getparam: IF line$(1)="" THEN RETurn
6750 IF type=0 OR type=8 THEN
6760 IF excess$="" THEN
6770 getsecond
6780 ELSE
6790 second$=excess$
6800 END IF
6810 identify seconds, type2, reg2: IF line$(1)="" THEN RETurn
6820 IF excess$<>"" OR type2<>0 THEN line$(1)="": RETurn
6830 IF type=0 THEN d2$=d2$+B
6840 IF type=8 THEN
6850 IF value<=B OR value>8 THEN line$(1)="": RETurn
6860 reg=lastd MOD 8
6870 END IF
6880 line$(1 TO 21)="1E" &hex$(reg*2+d2$ MOD 2)&hex$(size*4+d2$ DIV 4)&hex$(d2$ DIV 2 MOD 2*8+reg2)
6890 RETurn
6900 END IF
6910 IF excess$<>"" OR type=1 THEN line$(1)="": RETurn
6920 line (2 TO 5)="E" &d2$&hex$(12+type DIV 2)&hex$(type MOD 2*8+reg
6930 END DEFine
6940 DEFine PROCedure comB
6950 IF line$(28 TO 33)=="CHGA" THEN bitop 1: RETurn
6960 IF line$ (28 TO 33)=="CLRAAA" THEN bitop 2: RETurn
6970 IF line$(28 TO 33)=="SETAAA" THEN bitop 3: RETurn
6980 IF line$(28 TO 33)=="TSTAAA" THEN bitop 0:RETurn
6990 IF line$ (28 TO 29)=="SR" THEN
7000 cond=1
7010 ELSE
7020 IF line$ (28 TO 29)=="RA" THEN
7030 cond=
7040 ELSE
7050 condition 28
7060 IF cond<0 OR cond=1 THEN eror$="Invalid mnemonic": RETurn
7070 END IF
7080 END IF
7090 size=1
7100 getparam: IF line$(1)="" THEN RETurn
7110 IF excess$<>"" OR type<>7 OR reg<>2 THEN line$(1)="": RETurn
7120 line$(2 TO 5)="6"&hex$(cond)&"00"
7130 IF line$(30 TO 33)="AAA" THEN RETurn
7140 IF NOT(line$(30 TO 33)==".SA") THEN line$(1)="": RETurn
7150 line$(1)="1"
7160 IF line$(6)<>"x" THEN
7170 IF value=0 OR (value>255 AND value<65408) THEN line$(1)="": RETurn
7180 line$(4 TO 9)=hex$(INT (value/16) MOD 16)&hex$(value-16*INT(value/16))
7190 RETurn
7200 END IF
7210 line$(4 TO 9)="xx"
7220 END DEFine
7230 DEFine PROCedure condition(n)
7240 cond=-1
7250 letter=CODE(line$(n))
7260 IF letter>CODE("Z") THEN letter=letter-32
7270 SELect ON letter
7280 CODE("T"): IF line$(n+1)="":cond=0: RETurn
7290 CODE("F"): IF line$(n+1)="":cond=1:RETurn
7300 CODE("H")
7310 IF line$(n+1)=="I" THEN cond=2: RETurn
7320 IF line$(n+1)=="S" THEN cond=4: RETurn
7330 CODE("L")
7340 IF line$(n+1)=="S" THEN cond=3:RETurn
7350 IF line$(n+1)=="0" THEN cond=5: RETurn
7360 IF line$(n+1)=="T" THEN cond=13:RETurn
7370 IF line (n+1)=="E" THEN cond=15: RETurn
7380 CODE("C")
7390 IF lines(n+1)=="C" THEN cond=4: RETurn
7400 IF lines(n+1)=="S" THEN cond=5: RETurn
7410 CODE("N"): IF line$(n+1)=="E" THEN cond=6: RETurn
7420 CODE("E"): IF line$(n+1)=="Q" THEN cond=7:RETurn
7430 CODE("V")
7440 IF line$(n+1)=="C" THEN cond=8:RETurn
7450 IF line$(n+1)=="S" THEN cond=9:RETurn
7460 CODE("P"): IF line$(n+1)=="L" THEN cond=10:RETurn
7470 CODE("M"): IF line$(n+1)=="I" THEN cond=11:RETurn
7480 CODE("6")
7490 IF line$(n+1)=="E" THEN cond=12: RETurn
7500 IF line$(n+1)=="T" THEN cond=14: RETurn
7510 END SELect
7520 END DEFine

7530 DEFine PROCedure bitop (op)
7540 size=1
7550 get 2params: IF line$(1)="" THEN RETurn
7560 IF type2=1 OR type2>7 THEN line$(1)="": RETurn
7570 IF type=B THEN
7580 line$(2 TO 5)="08"&hex$(op*4+type2 DIV 2)&hex$(type2 MOD 2*8+r eg2)
7590 RETurn
7600 END IF
7610 IF type=0 THEN
7620 line$(2 TO 5)="0"&hex$(reg*2+1)&hex$(op*4+type2 DIV 2)&hex$(ty pe2 MOD 2*8+reg2)
7630 RETurn
7640 END IF
7650 line$(1)=""
7660 END DEFine

7670 DEFine PROCedure comC
7680 IF line$(28 TO 33)=="HKAAAA" THEN
7690 size=1
7700 get2params: IF line$(1)="" THEN RETurn
7710 IF type2<>0 OR type=1 OR type>8 THEN line$(1)="": RETurn
7720 IF type=B THEN type=7:reg=4
7730 line$(2 TO 5)="4"&hex$(reg2+2+1)&hex$(8+type DIV 2)&hex$(type MOD 2*8+reg)
7740 RETurn
7750 END IF
7760 IF line$(28 TO 29)=="LR" THEN
7770 size=1
7780 IF line$(30)="." THEN
7790 getsize 31:IF fault THEN RETurn
7800 ELSE
7810 IF line$(30 TO 33)<>"" THEN RETurn
7820 END IF
7830 getparam: IF line$(1)="" THEN RETurn
7840 IF excess$<>"" OR type=1 OR type>7 THEN line$(1)="": RETurn
7850 line$(2 TO 5)="42"&hex$(size*4+type DIV 2)&hex$(type MOD 2*8+reg)
7860 RETurn
7870 END IF
7880 IF line$(28 TO 29)=="MP" THEN
7890 size=1
7900 IF line$(30)="," THEN getsize 31: IF fault THEN RETurn
7910 IF line$(31)="," THEN getsize 32: IF fault THEN RETurn
7920 get2params: IF line$(1)="" THEN RETurn
7930 IF (line$(30)=="I" AND type>8) OR (line$(30)=="A" AND type2<>1) THEN line$(1)="": RETurn
7940 IF line$(30)=="M" THEN
7950 IF type<>3 OR type2<>3 THEN line$(1)="": RETurn
7960 line$(2 TO 5)="B"&hex$(reg2*2+1)&(size*4)&hex$(8+reg)
7970 RETurn
7980 END IF
7990 IF type2=1 THEN
8000 IF size=0 THEN line$(1)="": RETurn
8010 IF type=8 THEN type=7:reg=4
8020 line$(2 TO 5)="B"&hex$(reg2*2+size-1)&hex$(12+type DIV 2)&hex$(type MOD 2*8+reg)
8030 RETurn
8040 END IF
8050 IF type=8 THEN
8060 line$(2 TO 5)="BC"&hex$(size*4+type2 DIV 2)&hex$(type2 MOD 2*8+reg2)
8070 RETurn
8080 END IF
8090 IF type2=0 THEN
8100 line$(2 TO 5)="B"&hex$(reg2+2)&hex$(size*4+type DIV 2)&hex$(type MOD 2*8+reg)
8110 RETurn
8120 END IF
8130 line$(1)=""
8140 RETurn
8150 END IF
8160 eror$="Invalid mnemonic"
8170 END DEFine
8180 DEFine PROCedure comD
8190 IF line$(28)=="C" THEN constant: RETurn
8200 IF line$(28)=="S" THEN space: RETurn
8210 IF line$(29)=="B" THEN decbranch: RETurn
8220 IF line$(28 TO 29)=="IV" THEN divormul:RETurn
8230 eror$="Invalid mnemonic"
8240 END DEFine
8250 DEFine PROCedure constant
8260 size=1
8270 IF line$(29)="." THEN
8280 getsize 30: IF fault THEN RETurn
8290 ELSE
8300 IF line$(29 TO 33)<>"" THEN RETurn
8310 END IF
8320 CLS#0
8330 INPUT#0;\,,"Data"\(line$(22 TO 26)), (line$(27 TO 33)),in$
8340 IF in$="" THEN RETurn
8350 IF LEN(in$)>27 THEN eror$="Too long": RETurn
8360 line$(34 TO 60)=in$
8370 line$(1)=0
8380 itemsize=2^size
8390 datasize=0
8400 pointer=1
8410 REPeat parse
8420 IF pointer>LEN(in$) THEN EXIT parse
8430 IF in$(pointer)="$" OR line$(1)="" THEN skiphex: NEXT parse
8440 IF in$(pointer)='"' OR in$(pointer)='"' THEN skiptext: NEXT parse
8450 skipdec
8460 END REPeat parse
8470 IF in$(LEN(in$))="," THEN datasize=datasize+itemsize
8480 line$(2 TO 21)=datasize
8490 END DEFine
8500 DEFine PROCedure skiphex
8510 REPeat advhex
8520 pointer=pointer+1
8530 IF pointer>LEN(in$) THEN datasize=datasize+itemsize: RETurn
8540 IF (in$(pointer)>="0" AND in$(pointer) <="9") OR (in$(pointer) >="A" AND in$(pointer) <="F") OR in$(pointer)="-" THEN NEXT ad vhex
8550 IF in$(pointer)="," THEN datasize=datasize+itemsize:pointer=p ointer+1: RETurn
8560 line$(1)="": RETurn
8570 END REPeat advhex
8580 END DEFine
8590 DEFine PROCedure skipdec
8600 REPeat advdec
8610 IF pointer>LEN(in$) THEN datasize=datasize+itemsize: RETurn
8620 IF in$(pointer)="," THEN datasize=datasize+itemsize:pointer=p ointer+1: RETurn
8630 IF in$(pointer)="$" THEN skiphex: RETurn
8640 IF (in$(pointer) <"0" OR in$(pointer)>"9") AND in$(pointer)<>" -" THEN line$(1)="": RETurn
8650 pointer=pointer+1
8660 END REPeat advdec
8670 END DEFine
8680 DEFine PROCedure skiptext
8690 quote$=in$(pointer)
8700 startsize=datasize
8710 REPeat advtext
8720 pointer=pointer+1
8730 IF pointer>LEN(in$) THEN line$(1)="": RETurn
8740 IF in$(pointer)=quote$ THEN
8750 pointer=pointer+1
8760 IF pointer>LEN(in$) THEN EXIT advtext
8770 IF in$(pointer)="," THEN pointer=pointer+1: EXIT advtext
8780 END IF
8790 datasize=datasize+1
8800 END REPeat advtext
8810 IF datasize=startsize THEN datasize=datasize+itemsize
8820 datasize=itemsize*INT((datasize+itemsize-1)/itemsize)
8830 END DEFine
8840 DEFine PROCedure space
8850 size=1
8860 IF line$(29)="." THEN
8870 getsize 30: IF fault THEN RETurn
8880 ELSE
8890 IF line$(29 TO 33)<>"AAAAA" THEN RETurn
8900 END IF
8910 CLS#0
8920 INPUT#0;\,, "Space" \(line$(22 TO 26)), (line$ (27 TO 33)), sources
8930 IF source$="" THEN RETurn
8940 line$(34 TO 60)=source$
8950 line$(1)="0"
8960 IF source$(1)="$" THEN
8970 extracthex
8980 ELSE
8990 extractdecimal
9000 END IF
9010 IF source$<>"" OR value<0 THEN line$(1)="": RETurn
9020 line$(2 TO 21)=value*2^size
9030 END DEFine
9040 DEFine PROCedure decbranch
9050 IF line$ (29 TO 30)=="RA" THEN
9060 cond=1
9070 ELSE
9080 condition 29
9090 END IF
9100 IF cond<0 OR line$(31 TO 33)<>"" THEN eror$="Invalid mnemonic": RETurn
9110 size=1
9120 get2params: IF line$(1)="" THEN RETurn
9130 IF type<>0 OR type2<>7 OR reg2<>2 THEN line$(1)="": RETurn
9140 line$(2 TO 5)="5"&hex$(cond)&"C"&hex$(8+reg)
9150 END DEFine
9160 DEFine PROCedure divormul (d1$)
9170 sign--1
9180 IF line$(30 TO 33)=="SAAA"THEN sign=1
9190 IF line$(30 TO 33)=="UAAA"THEN sign=0
9200 IF sign 0 THEN eror$="invalid mnemonic":RETurn
9210 size=1
9220 get2params: IF line$(1)="" THEN RETurn
9230 IF type2<>0 OR type=1 OR type>8 THEN line$(1)="": RETurn
9240 IF type=B THEN type=7:reg=4
9250 line$(2 TO 5)=d1$&hex$(ireg2*2+sign)&hex$(12+type DIV 2)&hex$(type MOD 2*8+reg)
9260 END DEFine
9270 DEFine PROCedure comE
9280 IF line$(28 TO 29)=="OR" THEN logical: RETurn
9290 IF line$(28 TO 33)=="X6AAAA" THEN exchange: RETurn
9300 IF line$(28 TO 29)=="XT" THEN extend: RETurn
9310 eror$="Invalid mnemonic"
9320 END DEFine
9330 DEFine PROCedure exchange
9340 size=1
9350 get2params: IF line$(1)="" THEN RETurn
9360 IF type>1 OR type2>1 THEN line$(1)="": RETurn
9370 IF type=type2 THEN
9380 line$(2 TO 5)="C"&hex$(reg*2+1)&"4"&hex$(type*8+reg2)
9390 RETurn
9400 END IF
9410 IF type2=0 THEN
9420 line$(2 TO 5)="C"&hex$(reg2*2+1)&"B"&hex$(8+reg)
9430 RETurn
9440 END IF
9450 line$(2 TO 5)="C"&hex$(reg*2+1)&"8"&hex$(8+reg2)
9460 END DEFine
9470 DEFine PROCedure extend
9480 size=1
9490 IF line$(30)="." THEN
9500 getsize 31
9510 IF fault OR size=0 THEN RETurn
9520 ELSE
9530 IF line$(30 TO 33)<>"" THEN RETurn
9540 END IF
9550 getparam
9560 IF excess$<>"" OR line$(1)="" OR type<>0 THEN line$(1)="": RETurn
9570 line$(2 TO 5)="48"&hex$(4+4*size)&reg
9580 END DEFine
9590 DEFine PROCedure comJ
9600 d3$="0"
9610 IF line$(28 TO 33)=="MPAAAA" THEN d3$="12"
9620 IF line$(28 TO 33)=="SRAAAA" THEN d3$="8"
9630 IF d3$="0" THEN eror$="Invalid mnemonic": RETurn
9640 size=1
9650 getparam
9660 IF excess$<>"" OR line$(1)="" OR type<2 OR type=3 OR type=4 OR type>7 THEN line$(1)="": RETurn
9670 line$(2 TO 5)="4E"&hex$(d3$+type DIV 2)&hex$(type MOD 2*8+reg)
9680 END DEFine
9690 DEFine PROCedure comI
9700 IF line$(28 TO 33)=="EAAAAA" THEN
9710 size=1
9720 get2params: IF line$(1)="" THEN RETurn
9730 IF type<2 OR type=3 OR type=4 OR type>7 OR type2<>1 THEN line$(1)="": RETurn
9740 line$(2 TO 5)="4"&hex$(reg2*2+1)&hex$(12+type DIV 2)&hex$(type MOD 2*8+reg)
9750 RETurn
9760 END IF
9770 IF line$(28 TO 33)=="INKAAA" THEN
9780 size=1
9790 get2params: IF line$(1)="" THEN RETurn
9800 IF type<>1 OR type2<>8 THEN line$(1)="": RETurn
9810 line$(2 TO 5)="4E5"&reg
9820 RETurn
9830 END IF
9840 IF line$(28 TO 29)=="SL" OR line$(28 TO 29)=="SR" THEN shift:RETurn
9850 eror$="Invalid mnemonic"
9860 END DEFine
9870 DEFine PROCedure comM
9880 IF line$(28 TO 29)=="UL" THEN divormul "C": RETurn
9890 IF line$(28 TO 31)=="MOVEM" THEN multiple:RETurn
9900 IF line$(28 TO 31)=="MOVEP" THEN peripheral: RETurn
9910 IF line$(28 TO 30)=="OVE" THEN mov: RETurn
9920 eror$="Invalid mnemonic"
9930 END DEFine
9940 DEFine PROCedure multiple
9950 size=1
9960 IF line$(32)="." THEN
9970 getsize 33
9980 IF fault OR size=0 THEN RETurn
9990 ELSE
10000 IF line$(32 TO 33)<>"" THEN RETurn
10010 END IF
10020 line$(1)="2"
10030 CLS#0
10040 INPUT#0;\,, "Parameter"\(line$(22 TO 26)), (line$(27 TO 33)), in$
10050 IF in$="" OR LEN(in$)>27 THEN line$(1)="": RETurn
10060 line$(34 TO 60)=in$
10070 reglist (in$)
10080 IF type=B OR type=9 OR type=4 OR line$(1)="" THEN line$(1)="" : RETurn
10090 IF excess$="" THEN
10100 getsecond
10110 ELSE
10120 second$=excess$
10130 END IF
10140 IF type=10 THEN
10150 identify second, type, reg
10160 IF type=8 OR type=9 OR type=3 OR type 2 THEN line$(1)="": RETUrn
10170 d2$="8"
10180 ELSE
10190 temp=type
10200 reglist second$
10210 IF type<>10 THEN line$(1)="": RETurn
10220 type=temp
10230 d2$="C"
10240 END IF
10250 IF excess$<>"" OR line$(1)="" THEN line$(1)="": RETurn
10260 IF d2$="8" AND type=4 THEN
10270 value=dec(line$(6))*4096+dec(line$(7))*256+dec(line$(8))*16+dec(line$(9))
10280 newval=0
10290 FOR i=0 TO 15
10300 newval=newval*2+value-2*INT (value/2)
10310 value=INT(value/2)
10320 END FOR i
10330 temp$=line$(1)
10340 line$(1)="1"
10350 deposit newval, 1
10360 line$(1)=temp$
10370 END IF
10380 line$(2 TO 5)="4"&d2$&hex$(4+size+4+type DIV 2)&hex$(type MOD 2*8+reg)
10390 END DEFine

10400 DEFine PROCedure reglist (source$)
10410 value=0
10420 type=10
10430 pointer=1
10440 REPeat search
10450 flag=-1
10460 IF source$(pointer)=="D" THEN flag=0
10470 IF source$(pointer)=="A" THEN flag=8
10480 IF flag<0 OR LEN (source$)<2 THEN identify sources, type, reg: RETurn
10490 pointer=pointer+1
10500 IF source$(pointer)<"0" OR sources (pointer)>"7" THEN identify sources, type, reg: RETurn
10510 numb=source$(pointer)
10520 value=value+2^(flag+numb)
10530 pointer=pointer+1
10540 IF pointer>LEN (source) THEN EXIT search
10550 IF source$(pointer)="-" THEN
10560 IF sources (pointer+1)<>sources (pointer-2) THEN line$(1)=""R ETurn
10570 pointer=pointer+2
10580 IF source$(pointer)<"0" OR sources (pointer)>"7" THEN line$(1) ="4": RETurn
10590 FOR i=numb+1 TO sources (pointer)
10600 value=value+2^(flag+i)
10610 END FOR i
10620 pointer=pointer+1
10630 IF pointer>LEN (source$) THEN EXIT search
10640 END IF
10650 IF source$(pointer)="," THEN EXIT search
10660 IF source$(pointer)<>"/" THEN identify sources, type, reg: RETurn
10670 pointer=pointer+1
10680 IF pointer>LEN (source$) THEN line$(1)="": RETurn
10690 END REPeat search
10700 excess$=""
10710 IF LEN(source$) >pointer THEN excess$=source$(pointer+1 TO)
10720 temp$=line$(1)
10730 line$(1)="1"
10740 deposit value, 1
10750 line$(1)=temp$
10760 END DEFine

10770 DEFine FuNction dec (a$)
10780 IF a$<="9" THEN RETurn a$
10790 RETurn CODE (a$)-55
10800 END DEFine

10810 DEFine PROCedure peripheral
10820 size=1
10830 IF line$(32)="." THEN
10840 getsize 33
10850 IF fault OR size=0 THEN RETurn
10860 ELSE
10870 IF line$ (32 TO 33)<>"" THEN RETurn
10880 END IF
10890 get2params: IF line$(1)="" THEN RETurn
10900 IF type=5 THEN
10910 IF type2<>0 THEN line$(1)="": RETurn
10920 line$(2 TO 5)="0"&hex$(reg2+2+1)&(4*size-4)&hex$(8+reg)
10930 RETurn
10940 END IF
10950 IF type=0 THEN
10960 IF type2<>5 THEN line$(1)="": RETurn
10970 line$(2 TO 5)="0"&hex$(reg*2+1)&hex$(4*size+4)&hex$ (8+reg2)
10980 RETurn
10990 END IF
11000 line$(1)=""
11010 END DEFine
11020 DEFine PROCedure mov
11030 size=1
11040 IF line$(31)=="Q" THEN size=0
11050 IF line$(31)="." THEN getsize 32: IF fault THEN RETurn
11060 IF line$(32)="." THEN getsize 33: IF fault THEN RETurn
11070 get2params: IF line$(1)="" THEN RETurn
11080 IF line$(31)=="Q" AND (type<>8 OR type2<>0) THEN line$(1)="":R ETurn
11090 IF line$(31)=="A" AND type2<>1 THEN line$ (1)="": RETurn
11100 IF type=9 THEN
11110 IF type2=1 OR type2>7 THEN line$(1)="": RETurn
11120 line$(2 TO 5)="40"&hex$(12+type2 DIV 2)&hex$(type2 MOD 2*8+reg 2)
11130 RETurn
11140 END IF
11150 IF type=8 THEN type=7:reg=4
11160 IF type2=9 THEN
11170 IF type=1 THEN line$(1)="": RETurn
11180 line$(2 TO 5)="4"&(2*reg2+4)&hex$(12+type DIV 2)&hex$(type MOD 2*8+B+reg)
11190 RETurn
11200 END IF
11210 IF type=1 AND type2=1 THEN
11220 IF reg 7 AND line$(34)=="U" THEN
11230 line$(2 TO 5)="4E6"&hex$(8+reg2)
11240 RETurn
11250 END IF
11260 IF reg2=7 AND line$(37)=="U" THEN
11270 line$(2 TO 5)="4E6"&reg
11280 RETurn
11290 END IF
11300 END IF
11310 IF type=7 AND reg=4 AND type2=0 AND (line$(31)=="Q" OR (size=2 AND (value<128 OR value>=2^32-128))) THEN
11320 line$(1 TO 13)="17"&hex$(reg2*2)&line$(dsize*4+4 TO dsize+4+9)
11330 RETurn
11340 END IF
11350 IF size<2 THEN size=size*2+1
11360 line$(2 TO 5)=size&hex$(reg2+2+type2 DIV 4)&hex$(type2 MOD 4+4+ type DIV 2)&hex$(type MOD 2+8+reg)
11370 END DEFine
11380 DEFine PROCedure comN
11390 IF line$(28 TO 33)=="OPAAAA" THEN line$(1 TO 5)="14E71": RETurn
11400 IF line$(28 TO 33)=="BCDAAA" THEN unary 8,0: RETurn
11410 size=1
11420 IF line$(30)="," THEN getsize 31: IF fault THEN RETurn
11430 IF line$(31)="." THEN getsize 32: IF fault THEN RETurn
11440 IF line$(28 TO 30)=="E6X" THEN unary 0, size: RETurn
11450 IF line$(28 TO 29)=="EG" THEN unary 4, size:RETurn
11460 IF line$(28 TO 29)=="OT" THEN unary 6, size: RETurn
11470 eror$="Invalid mnemonic"
11480 END DEFine

11490 DEFine PROCedure unary (d2$,size)
11500 getparam
11510 IF excess$<>"" OR line$(1)="" OR type=1 OR type>7 OR (type=7 AND reg>1) THEN line$(1)="": RETurn
11520 line$(2 TO 5)="4"&d2$&hex$(size*4+type DIV 2)&hex$(type MOD 2+8+reg)
11530 END DEFine

11540 DEFine PROCedure comO
11550 IF line$(28)="R" THEN logical: RETurn
11560 eror$="Invalid mnemonic"
11570 END DEFine

11580 DEFine PROCedure comP
11590 IF line$(28 TO 33)=="EAAAAA" THEN
11600 size=1
11610 getparam
11620 IF excess$<>"" OR line$(1)="" OR type<2 OR type=3 OR type=4 OR type>7 THEN line$(1)="": RETurn
11630 line$(2 TO 5)="48"&hex$(4+type DIV 2)&hex$(type MOD 2+8+reg)
11640 RETurn
11650 END IF
11660 eror$="Invalid mnemonic"
11670 END DEFine

11680 DEFine PROCedure comR
11690 IF line$(28)=="O" THEN IF line$(29)=="L" OR line$(29)=="R" OR line$(29 TO 30)=="XL" OR line$(29 TO 30)=="XR" THEN shift:RETurn
11700 IF line$ (28 TO 33)=="ESETA" THEN line$(1 TO 5)="14E70": RETurn
11710 IF line$ (28 TO 33)=="TEAAAA THEN line$(1 TO 5)="14E73": RETurn
11720 IF line$(28 TO 33)=="TRAAAA" THEN line$(1 TO 5)="14E77": RETurn
11730 IF line$(28 TO 33)=="TSAAAA" THEN line$(1 TO 5)="14E75": RETurn
11740 eror$="Invalid mnemonic"
11750 END DEFine

11760 DEFine PROCedure comS
11770 IF line$(28 TO 33)=="BCDA" THEN ext 8,0: RETurn
11780 IF line$ (28 TO 33)=="TOP" THEN
11790 size=1
11800 getparam
11810 IF excess$<>"" OR line$(1)="" OR type<>8 THEN line$(1)="": RETurn
11820 line$(2 TO 5)="4E72"
11830 RETurn
11840 END IF
11850 IF line$ (28 TO 29)=="UB" THEN addorsub:RETurn
11860 IF line$ (28 TO 33)=="SWAPAA" THEN
11870 size=1
11880 getparam
11890 IF excess$<>"" OR line$(1)="" OR type<>0 THEN line$(1)="": RETurn
11900 line$(2 TO 5)="484"&reg
11910 RETurn
11920 END IF
11930 condition 28
11940 IF cond<0 OR line$(30 TO 33)<>"AAA" THEN eror$="Invalid mnemonic": RETurn
11950 size=1
11960 getparam
11970 IF excess$<>"" OR line$(1)="" OR type=1 OR type>7 OR (type=7 AND reg>1) THEN line$(1)="": RETurn
11980 line$(2 TO 5)="5"&hex$(cond) &hex$(12+type DIV 2) &hex$(type MOD 2*8+reg)
11990 END DEFine

12000 DEFine PROCedure comT
12010 IF line$(28 TO 33)=="TASAAA" THEN unary "A", 3: RETurn
12020 IF line$ (28 TO 33)=="TRAPV" THEN line$(1 TO 5)="14E76": RETurn
12030 IF line$(28 TO 33)=="TRAP" THEN
12040 size=1
12050 getparam
12060 IF excess$<>"" OR line$(1)="" OR type<>8 OR value>15 THEN line$(1)="": RETurn
12070 line$(1 TO 9)="14E4"&hex$(value)
12080 RETurn
12090 END IF
12100 IF line$(28 TO 29)=="ST" THEN
12110 size=1
12120 IF line$(30)="," THEN
12130 getsize 31: IF fault THEN RETurn
12140 ELSE
12150 IF line$(30 TO 33)<>"" THEN RETurn
12160 END IF
12170 unary "A",size
12180 RETurn
12190 END IF
12200 eror$="Invalid mnemonic"
12210 END DEFine
12220 DEFine PROCedure comU
12230 IF line$(28 TO 33)=="NLKAAA" THEN
12240 size=1
12250 getparam
12260 IF excess$<>"" OR line$(1)="" OR type<>1 THEN line$(1)="": RETurn
12270 line$(2 TO 5)="4E5"&hex$(8+reg)
12280 RETurn
12290 END IF
12300 eror$="Invalid mnemonic"
12310 END DEFine
12320 DEFine PROCedure assemble
12330 CLS#0
12340 IF codespace>0 THEN
12350 PRINT #0;\"Use the currently reserved space?"
12360 d$=INKEY$(#0,-1)
12370 ELSE
12380 d$="N"
12390 END IF
12400 res=-1
12410 IF d$=="N" THEN
12420 CLS#0
12430 INPUT#0;\"How many spare bytes would you like?" \ress:res="0"&r
es$
12440 END IF
12450 IF prog$<>"" THEN
12460 CLS#0
12470 PRINT#0; \"Assemble the current program?"
12480 d$=INKEY$(#0,-1)
12490 ELSE
12500 d$="N"
12510 END IF
12520 name$=""
12530 IF d$=="N" THEN
12540 REPeat getname
12550 CLS#0
12560 INPUT#0; \"Drive number"!d$:d="0"&d$: IF d=0 THEN EXIT getname
12570 INPUT#0; "File name"! fname$
12580 name$=name$&FILL$(",", 38)
12590 n=LEN(name$)
12600 name$(n-37 TO)=CHR$(LEN(fname$))&"mdv"&d&" "&fname$
12610 END REPeat getname
12620 END IF
12630 curaddr=0
12640 pass1
12650 IF fault THEN editor: RETurn
12660 IF res=0 OR need codespace THEN
12670 codespace need: IF res>0 THEN codespace codespace+res
12680 codespace=16*INT((codespace+15)/16)
12690 codeaddr=RESPR(codespace)
12700 PRINT "New space reserved at:"codeaddr
12710 END IF
12720 pass2
12730 IF fault THEN
12740 IF name$="" THEN
12750 iline=curline-1
12760 topline=iline-10
12770 END IF
12780 PRINT#0;\"Press any key for editor"
12790 codesize=0
12800 PAUSE
12810 editor
12820 RETurn
12830 END IF
12840 codesize=need
12850 PRINT#0;\"Assembly complete press any key"
12860 PAUSE
12870 END DEFine

12880 DEFine PROCedure pass1
12890 CSIZE 2,0:CLS: CLS#0: PRINT "Pass 1-Labels"\\
12900 curline=0
12910 need=0
12920 fault=0
12930 label=""
12940 REPeat read1
12950 readline: IF line$="" THEN RETurn
12960 IF need MOD 2=1 THEN
12970 IF line$(1)="0" THEN
12980 IF line$(22)="*" THEN NEXT read1
12990 IF line$(27)=="D" THEN
13000 IF line$(29 TO 30)<>".B" AND line$(29 TO 30)<>".b" THEN need =need+1
13010 ELSE
13020 need=need+1
13030 END IF
13040 ELSE
13050 need=need+1
13060 END IF
13070 END IF
13080 IF line$(22)<>"+" AND line$(22 TO 26)<>"AAA" THEN
13090 findlabel line$(22 TO 26)
13100 IF label>0 THEN
13110 fault=1
13120 IF name$="" THEN
13130 iline=curline-1
13140 topline=iline-10
13150 END IF
13160 PRINT line$(22 TO 26), "repeated at line"!curline-1
13170 PRINT#0;\"Press any key for editor"
13180 PAUSE
13190 RETurn
13200 END IF
13210 newlabel$=line$(22 TO 26)&hex$(need DIV 4096)&hex$(need DIV 256 MOD 16)&hex$(need DIV 16 MOD 16)&hex$(need MOD 16)
13220 PRINT newlabel$(1 TO 5), newlabel$(6 TO 9), "At line"!curline-1
13230 label$=label$&newlabel$
13240 END IF
13250 IF line$(1)="0" THEN
13260 need=need+line$(2 TO 21)
13270 ELSE
13280 need=need+2*line$(1)
13290 END IF
13300 END REPeat read1: END DEFine
13310 DEFine PROCedure readline
13320 IF name$="" THEN
13330 curline=curline+1
13340 IF curline>LEN(prog$)/60 THEN line$="": RETurn
13350 line$=prog$(curline*60-59 TO curline*60)
13360 RETurn
13370 END IF
13380 IF curline=0 THEN
13390 curline=1
13400 PRINT "File:"; name$(2 TO 38): IF curaddr>0 THEN PRINT "AAAAAA"
13410 OPEN#4;name$(2 TO 6+CODE(name$))
13420 file=1
13430 END IF
13440 curline=curline+1
13450 IF EOF(#4) THEN
13460 CLOSE#4
13470 file=file+1
13480 IF file>LEN(name$)/38 THEN line$="": RETurn
13490 PRINT "File:"; name$(file*38-36 TO file*38): IF curaddr>0 THEN PRINT "AAAAAA";
13500 OPEN#4;name$(file*38-36 TO file*38-32+CODE(name$(file*38-37)))
13510 curline=1
13520 END IF
13530 INPUT#4; line$
13540 END DEFine
13550 DEFine PROCedure findlabel (a$)
13560 label=1
13570 REPeat find
13580 IF label>LEN(label$)/9 THEN EXIT find
13590 IF label$(label*9-8 TO label*9-4)==a$ THEN RETurn
13600 label=label+1
13610 END REPeat find
13620 label=0
13630 END DEFine
13640 DEFine PROCedure pass2
13650 PRINT \\"Pass 2*\\:CSIZE 0,0
13660 curline=0
13670 fault=0
13680 curaddr=codeaddr
13690 REPeat read2
13700 PRINT hex5$(curaddr)!;
13710 readline
13720 IF line$="" THEN PRINT...., "END": RETurn
13730 IF line$(22)="** THEN PRINT,,,,line$(22 TO 60): NEXT read2
13740 IF curaddr<>2*INT(curaddr/2) THEN
13750 IF line$(1)<>"0" OR NOT (line$(29 TO 30)==".B") THEN POKE curaddr, 0:curaddr=curaddr+1: PRINT "00"\hex5$(curaddr)!;
13760 END IF
13770 IF line$(1)="0" THEN
13780 IF line$(28)=="S" THEN
13790 PRINT,,,,line$(22 TO 26), line$(27 TO 33), line$(34 TO 59)
13800 FOR i=1 TO line$(2 TO 21)
13810 POKE curaddr, 0
13820 curaddr=curaddr+1
13830 END FOR i
13840 NEXT read2
13850 END IF
13860 pokeconst
13870 IF fault THEN RETurn
13880 NEXT read2
13890 END IF
13900 FOR i=1 TO line$(1)
13910 IF line$(i+4)="x" THEN
13920 pokelabel
13930 IF fault THEN RETurn
13940 ELSE
13950 PRINT line$(i*4-2 TO i*4+1);
13960 POKE_W curaddr,dec(line$(i*4-2))*4096+dec(line$(i*4-1))*256+d
ec(line$(i*4))+16+dec(line$(i*4+1))
13970 END IF
13980 curaddr=curaddr+2
13990 END FOR i
14000 FOR i=1 TO 6-line$(1)
14010 PRINT "AAA";
14020 END FOR i
14030 PRINT,line$(22 TO 26), line$(27 TO 33), line$(34 TO 59)
14040 END REPeat read2
14050 END DEFine
14060 DEFine PROCedure pokeconst
14070 size=1
14080 IF line$(29)="." THEN getsize 30
14090 source$=", "&line$(34 TO 60)&","
14100 REPeat const
14110 IF source$(1)="" THEN EXIT const
14120 IF source$(1 TO 2)",," OR source$(1 TO 2)="" THEN
14130 FOR i=1 TO 2^size
14140 POKE curaddr,0
14150 PRINT "00";
14160 curaddr=curaddr+1
14170 END FOR i
14180 source=source$(2 TO)
14190 NEXT const
14200 END IF
14210 IF source$(1)<>",* THEN
14220 fault=1
14230 PRINT \"Incorrect syntax-"!line$(27 TO 60)
14240 PRINT#0;\"Press any key for editor"
14250 RETurn
14260 END IF
14270 source=source$(2 TO)
14280 IF source$(1)="" OR source$(1)='"' THEN
14290 scantext
14300 NEXT const
14310 END IF
14320 IF source$(1)="$" THEN
14330 ELSE
14340 extracthex
14350 extractdecimal
14360 END IF
14370 IF size=2 THEN
14380 IF lastd<4 THEN value=value+4
14390 IF lastd>=12 THEN value=value-4
14400 END IF
14410 IF value<0 THEN value=value+256^(2^size)
14420 IF value<0 OR value>=256^(2^size) THEN
14430 fault=1
14440 PRINT \"Value outside range in:"!line$(27 TO 60)
14450 RETurn
14460 END IF
14470 FOR i=2^size TO 1 STEP -1
14480 byte=INT(value/256^(i-1)-256+INT(value/256^i))
14490 IF size=2 THEN IF i=1 THEN byte=byte DIV 16*16+lastd
14500 POKE curaddr, byte
14510 PRINT hex$(byte DIV 16);hex$(byte MOD 16);
14520 curaddr=curaddr+1
14530 END FOR i
14540 END REPeat const
14550 PRINT,,,,line$(22 TO 26), line$(27 TO 33), line$(34 TO 59)
14560 END DEFine

14570 DEFine PROCedure scantext
14580 quote$=source$(1)
14590 datasize=0
14600 REPeat scan
14610 source$=source$(2 TO)
14620 IF source$(1)=quote$ THEN
14630 source$=source$(2 TO)
14640 IF source$(1)<>quote$ THEN
14650 missing=2^size*INT((datasize+2^size-1)/2^size)-datasize
14660 IF datasize=0 THEN missing=2^size
14670 FOR i=1 TO missing
14680 POKE curaddr,0
14690 curaddr=curaddr+1
14700 PRINT "00";
14710 END FOR i
14720 RETurn
14730 END IF
14740 END IF
14750 POKE curaddr, CODE (source$)
14760 curaddr=curaddr+1
14770 datasize=datasize+1
14780 PRINT hex$(CODE (source$) DIV 16); hex$(CODE (source$) MOD 16);
14790 END REPeat scan
14800 END DEFine
14810 DEFine PROCedure pokelabel
14820 IF 1=1 AND line$(2)="5" THEN
14830 source$=line$(37 TO 41)
14840 ELSE
14850 source$=line$(34 TO 38)
14860 END IF
14870 FOR j=2 TO 5
14880 IF source$(j)="(" OR source$(j)="," THEN source$ (j TO 5)="": EX IT j
14890 END FOR j
14900 findlabel source$
14910 IF label=0 THEN fault=1:PRINT \"Undefined label-"!line$(27 TO 60): RETurn
14920 jump=dec(label$(label*9-8))*4096+dec (label$(label*9-7))*256+dec <label$(label*9-6))*16+dec (label$(label*9-5))+codeaddr-curaddr
14930 IF 1=1 AND line$(2)="6" THEN jump=jump-2: IF jump<0 THEN fault=1 PRINT "Short jump.range=0-"!line$ (27 TO 60): RETurn
14940 IF line$(i*4-2)="x" THEN
14950 IF jump<-32768 OR jump>32767 THEN fault=1: PRINT \"Range error. -"line$(27 TO 60): RETurn
14960 IF jump<0 THEN jump=jump+65536
14970 PRINT hex$(INT(jump/4096)); hex$(INT (jump/256) MOD 16); hex$(INT( jump/16)MOD 16); hex$(jump-16*INT(jump/16));
14980 POKE_W curaddr, jump
14990 RETurn
15000 END IF
15010 IF jump<-128 OR jump>127 THEN fault=1: PRINT \"Range error-"!li ne$(27 TO 60): RETurn
15020 IF jump<0 THEN jump=jump+256
15030 PRINT line$(i*4-2 TO i*4-1); hex$(jump DIV 16); hex$(jump MOD 16) ;
15040 POKE curaddr, dec(line$(i*4-2))*16+dec(line$(i*4-1))
15050 POKE curaddr+1, jump
15060 END DEFine
15070 DEFine FuNction hex5$(curaddr)
15080 RETurn hex$(INT(curaddr/65536) MOD 16)&hex$(INT (curaddr/4096) MO D 16)&hex$(INT(curaddr/256) MOD 16)&hex$(INT(curaddr/16)-16*INT (curaddr/256)) & hex$(curaddr-16 INT(curaddr/16))
15090 END DEFine
15100 REMark End of Assembler