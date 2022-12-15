	.arch armv8-a
	.file	"loop-crc-calculation-check-fail.c"
	.text
	.section	.rodata
	.align	3
	.type	crc_32_tab, %object
	.size	crc_32_tab, 2048
crc_32_tab:
	.xword	0
	.xword	1996959894
	.xword	3993919788
	.xword	2567524794
	.xword	124634137
	.xword	1886057615
	.xword	3915621685
	.xword	2657392035
	.xword	249268274
	.xword	2044508324
	.xword	3772115230
	.xword	2547177864
	.xword	162941995
	.xword	2125561021
	.xword	3887607047
	.xword	2428444049
	.xword	498536548
	.xword	1789927666
	.xword	4089016648
	.xword	2227061214
	.xword	450548861
	.xword	1843258603
	.xword	4107580753
	.xword	2211677639
	.xword	325883990
	.xword	1684777152
	.xword	4251122042
	.xword	2321926636
	.xword	335633487
	.xword	1661365465
	.xword	4195302755
	.xword	2366115317
	.xword	997073096
	.xword	1281953886
	.xword	3579855332
	.xword	2724688242
	.xword	1006888145
	.xword	1258607687
	.xword	3524101629
	.xword	2768942443
	.xword	901097722
	.xword	1119000684
	.xword	3686517206
	.xword	2898065728
	.xword	853044451
	.xword	1172266101
	.xword	3705015759
	.xword	2882616665
	.xword	651767980
	.xword	1373503546
	.xword	3369554304
	.xword	3218104598
	.xword	565507253
	.xword	1454621731
	.xword	3485111705
	.xword	3099436303
	.xword	671266974
	.xword	1594198024
	.xword	3322730930
	.xword	2970347812
	.xword	795835527
	.xword	1483230225
	.xword	3244367275
	.xword	3060149565
	.xword	1994146192
	.xword	31158534
	.xword	2563907772
	.xword	4023717930
	.xword	1907459465
	.xword	112637215
	.xword	2680153253
	.xword	3904427059
	.xword	2013776290
	.xword	251722036
	.xword	2517215374
	.xword	3775830040
	.xword	2137656763
	.xword	141376813
	.xword	2439277719
	.xword	3865271297
	.xword	1802195444
	.xword	476864866
	.xword	2238001368
	.xword	4066508878
	.xword	1812370925
	.xword	453092731
	.xword	2181625025
	.xword	4111451223
	.xword	1706088902
	.xword	314042704
	.xword	2344532202
	.xword	4240017532
	.xword	1658658271
	.xword	366619977
	.xword	2362670323
	.xword	4224994405
	.xword	1303535960
	.xword	984961486
	.xword	2747007092
	.xword	3569037538
	.xword	1256170817
	.xword	1037604311
	.xword	2765210733
	.xword	3554079995
	.xword	1131014506
	.xword	879679996
	.xword	2909243462
	.xword	3663771856
	.xword	1141124467
	.xword	855842277
	.xword	2852801631
	.xword	3708648649
	.xword	1342533948
	.xword	654459306
	.xword	3188396048
	.xword	3373015174
	.xword	1466479909
	.xword	544179635
	.xword	3110523913
	.xword	3462522015
	.xword	1591671054
	.xword	702138776
	.xword	2966460450
	.xword	3352799412
	.xword	1504918807
	.xword	783551873
	.xword	3082640443
	.xword	3233442989
	.xword	3988292384
	.xword	2596254646
	.xword	62317068
	.xword	1957810842
	.xword	3939845945
	.xword	2647816111
	.xword	81470997
	.xword	1943803523
	.xword	3814918930
	.xword	2489596804
	.xword	225274430
	.xword	2053790376
	.xword	3826175755
	.xword	2466906013
	.xword	167816743
	.xword	2097651377
	.xword	4027552580
	.xword	2265490386
	.xword	503444072
	.xword	1762050814
	.xword	4150417245
	.xword	2154129355
	.xword	426522225
	.xword	1852507879
	.xword	4275313526
	.xword	2312317920
	.xword	282753626
	.xword	1742555852
	.xword	4189708143
	.xword	2394877945
	.xword	397917763
	.xword	1622183637
	.xword	3604390888
	.xword	2714866558
	.xword	953729732
	.xword	1340076626
	.xword	3518719985
	.xword	2797360999
	.xword	1068828381
	.xword	1219638859
	.xword	3624741850
	.xword	2936675148
	.xword	906185462
	.xword	1090812512
	.xword	3747672003
	.xword	2825379669
	.xword	829329135
	.xword	1181335161
	.xword	3412177804
	.xword	3160834842
	.xword	628085408
	.xword	1382605366
	.xword	3423369109
	.xword	3138078467
	.xword	570562233
	.xword	1426400815
	.xword	3317316542
	.xword	2998733608
	.xword	733239954
	.xword	1555261956
	.xword	3268935591
	.xword	3050360625
	.xword	752459403
	.xword	1541320221
	.xword	2607071920
	.xword	3965973030
	.xword	1969922972
	.xword	40735498
	.xword	2617837225
	.xword	3943577151
	.xword	1913087877
	.xword	83908371
	.xword	2512341634
	.xword	3803740692
	.xword	2075208622
	.xword	213261112
	.xword	2463272603
	.xword	3855990285
	.xword	2094854071
	.xword	198958881
	.xword	2262029012
	.xword	4057260610
	.xword	1759359992
	.xword	534414190
	.xword	2176718541
	.xword	4139329115
	.xword	1873836001
	.xword	414664567
	.xword	2282248934
	.xword	4279200368
	.xword	1711684554
	.xword	285281116
	.xword	2405801727
	.xword	4167216745
	.xword	1634467795
	.xword	376229701
	.xword	2685067896
	.xword	3608007406
	.xword	1308918612
	.xword	956543938
	.xword	2808555105
	.xword	3495958263
	.xword	1231636301
	.xword	1047427035
	.xword	2932959818
	.xword	3654703836
	.xword	1088359270
	.xword	936918000
	.xword	2847714899
	.xword	3736837829
	.xword	1202900863
	.xword	817233897
	.xword	3183342108
	.xword	3401237130
	.xword	1404277552
	.xword	615818150
	.xword	3134207493
	.xword	3453421203
	.xword	1423857449
	.xword	601450431
	.xword	3009837614
	.xword	3294710456
	.xword	1567103746
	.xword	711928724
	.xword	3020668471
	.xword	3272380065
	.xword	1510334235
	.xword	755167117
	.text
	.align	2
	.global	updcrc3
	.type	updcrc3, %function
updcrc3:
.LFB0:
	.cfi_startproc
	str	x19, [sp, -48]!
	.cfi_def_cfa_offset 48
	.cfi_offset 19, -48
	str	x0, [sp, 24]
	str	w1, [sp, 20]
	ldr	x0, [sp, 24]
	cmp	x0, 0
	bne	.L2
	mov	x19, 4294967295
	b	.L3
.L2:
	adrp	x0, crc.0
	add	x0, x0, :lo12:crc.0
	ldr	x19, [x0]
	ldr	w0, [sp, 20]
	str	w0, [sp, 44]
	ldr	w0, [sp, 20]
	cmp	w0, 0
	beq	.L3
.L4:
	ldr	x0, [sp, 24]
	add	x1, x0, 1
	str	x1, [sp, 24]
	ldrb	w0, [x0]
	and	x0, x0, 255
	eor	x0, x19, x0
	and	x1, x0, 255
	adrp	x0, crc_32_tab
	add	x0, x0, :lo12:crc_32_tab
	ldr	x1, [x0, x1, lsl 3]
	lsr	x0, x19, 8
	eor	x19, x1, x0
	ldr	w0, [sp, 20]
	sub	w0, w0, #1
	str	w0, [sp, 20]
	ldr	w0, [sp, 20]
	cmp	w0, 999
	bls	.L4
.L3:
	adrp	x0, crc.0
	add	x0, x0, :lo12:crc.0
	str	x19, [x0]
	eor	x0, x19, 4294967295
	ldr	x19, [sp], 48
	.cfi_restore 19
	.cfi_def_cfa_offset 0
	ret
	.cfi_endproc
.LFE0:
	.size	updcrc3, .-updcrc3
	.data
	.align	3
	.type	crc.0, %object
	.size	crc.0, 8
crc.0:
	.xword	4294967295
	.ident	"GCC: (Kunpeng gcc 10.3.1-2.3.0.b006) 10.3.1"
	.section	.note.GNU-stack,"",@progbits
