	DO (1) NEXT
	PLEASE INITIALIZE
(1)	DO FORGET #1
	DO (50000) NEXT DO NOTE INITIALIZE I/O
	DO (51000) NEXT DO NOTE INITIALIZE LINE INPUT
	DO (60000) NEXT DO NOTE INITIALIZE DATABASE
	DO (61000) NEXT DO NOTE INITIALIZE STATE
	DO (2) NEXT
	PLEASE STOP

	PLEASE WELCOME PLAYER
(2)	DO FORGET #1
	DO .1 <- #65
	DO (55601) NEXT DO NOTE RSPEAK
	DO (57101) NEXT DO NOTE YES
	DO (3) NEXT
	DO (5) NEXT
	PLEASE STOP

	PLEASE MARK HINT #3 HAS HINTED AND BUMP LAMP LIFETIME UP TO #1000
(3)	DO (4) NEXT
	DO FORGET #1
	DO .1 <- #1
	DO (55601) NEXT DO NOTE RSPEAK
	DO NOTE mark hint 3 as hinted
	...
	DO ,61000SUB#5 <- #1000 DO NOTE LIMIT
	DO (5) NEXT
	PLEASE STOP
(4)	DO RESUME .1
	PLEASE STOP

	PLEASE NOTE CAN'T LEAVE CAVE ONCE IT'S CLOSING (EXCEPT BY MAIN OFFICE).
(5)	DO FORGET #1
	DO NOTE ... NOT IMPLEMENTED YET
	DO (31) NEXT

	PLEASE SEE IF A DWARF HAS SEEN HIM AND HAS COME FROM WHERE HE WANTS TO
	       GO.  IF SO, THE DWARF'S BLOCKING HIS WAY.  IF COMING FROM PLACE
	       FORBIDDEN TO PIRATE (DWARVES ROOTED IN PLACE) LET HIM GET OUT
	       (AND ATTACKED).
(31)	DO FORGET #1
	DO NOTE ... NOT IMPLEMENTED YET
	DO (51) NEXT

	PLEASE FIRST OFF, DON'T LET THE DWARVES FOLLOW HIM INTO A PIT OR A
	       WALL.  ACTIVATE THE WHOLE MESS THE FIRST TIME HE GETS AS FAR AS
	       THE HALL OF MISTS (LOC 15).  IF NEWLOC IS FORBIDDEN TO PIRATE
	       (IN PARTICULAR, IF IT'S BEYOND THE TROLL BRIDGE), BYPASS DWARF
	       STUFF.  THAT WAY PIRATE CAN'T STEAL RETURN TOLL, AND DWARVES
	       CAN'T MEET THE BEAR.  ALSO MEANS DWARVES WON'T FOLLOW HIM INTO
	       DEAD END IN MAZE, BUT C'EST LA VIE.  THEY'LL WAIT FOR HIM
	       OUTSIDE THE DEAD END.
(51)	DO FORGET #1
	DO ,61000SUB#1 <- ,61000SUB#2 DO NOTE LOC <- NEWLOC
	DO NOTE ... NOT IMPLEMENTED YET
	DO (101) NEXT

	PLEASE WHEN WE ENCOUNTER THE FIRST DWARF, WE KILL 0, 1, OR 2 OF THE 5
	       DWARVES.  IF ANY OF THE SURVIVORS IS AT LOC, REPLACE HIM WITH
	       THE ALTERNATE.
	DO NOTE ... NOT IMPLEMENTED YET

	PLEASE THINGS ARE IN FULL SWING.  MOVE EACH DWARF AT RANDOM, EXCEPT IF
	       HE'S SEEN US HE STICKS WITH US.  DWARVES NEVER GO TO LOCS <15.
	       IF WANDERING AT RANDOM, THEY DON'T BACK UP UNLESS THERE'S NO
	       ALTERNATIVE.  IF THEY DON'T HAVE TO MOVE, THEY ATTACK.  AND, OF
	       COURSE, DEAD DWARVES DON'T DO MUCH OF ANYTHING.
	DO NOTE ... NOT IMPLEMENTED YET

	PLEASE THE PIRATE'S SPOTTED HIM.  HE LEAVES HIM ALONE ONCE WE'VE FOUND
	       CHEST.  COUNTS IF A TREASURE IS HERE.  IF NOT, AND TALLY=TALLY2
	       PLUS ONE FOR AN UNSEEN CHEST, LET THE PIRATE BE SPOTTED.  USE
	       PLACE(MESSAG) TO DETERMINE IF PIRATE'S BEEN SEEN, SINCE
	       PLACE(CHEST)=0 COULD MEAN HE THREW IT TO TROLL.
	DO NOTE ... NOT IMPLEMENTED YET

	PLEASE THIS THREATENING LITTLE DWARF IS IN THE ROOM WITH HIM!
	DO NOTE ... NOT IMPLEMENTED YET

	PLEASE NOW WE KNOW WHAT'S HAPPENING.  LET'S TELL THE POOR SUCKER ABOUT
	       IT.
	DO NOTE ... NOT IMPLEMENTED YET

	PLEASE DESCRIBE THE CURRENT LOCATION AND (MAYBE) GET NEXT COMMAND.
(101)	DO FORGET #1
	DO .1 <- ,61000SUB#1 DO NOTE .1 <- LOC
	DO NOTE IF LOC=#0, DEAD
	DO (102) NEXT
	DO (104) NEXT
	PLEASE STOP
(102)	DO (103) NEXT
	PLEASE ... NOT IMPLEMENTED YET: GOTO "YOU'RE DEAD, JIM."
	PLEASE STOP
(103)	DO RESUME '∀"'#65535~.1'~#1"¢#1'~#3
	PLEASE STOP
(104)	DO FORGET #1
	DO NOTE ... NOT IMPLEMENTED YET: IF DARK AND WZDARK %35 GOTO "YOU'RE DEAD, JIM."
	DO NOTE ... NOT IMPLEMENTED YET: IF TOTING #35 (BEAR), ARBITRARY MESSAGE #141
	DO NOTE ... NOT IMPLEMENTED YET: IF DARK, ARBITRARY MESSAGE #16
	DO NOTE INCREMENT ABB(LOC).
	DO .1 <- ,61001SUB",61000SUB#1"#1 DO NOTE .1 <- ABB(LOC).
	DO .2 <- ,61000SUB#9 DO NOTE .2 <- ABBNUM
	DO (40001) NEXT DO NOTE .3 <- .1 MOD .2
	DO .1 <- ,61000SUB#1 DO NOTE .1 <- LOC
	DO (105) NEXT
	DO (55201) NEXT DO NOTE SPEAK(STEXT(LOC)).
	DO (107) NEXT
	PLEASE STOP
(105)	DO (106) NEXT
	DO FORGET #1
	DO (55101) NEXT DO NOTE SPEAK(LTEXT(LOC)).
	DO (107) NEXT
	PLEASE STOP
(106)	DO RESUME "∀'"#65535~.3"~#1'¢#1"~#3
	PLEASE STOP
(107)	DO FORGET #1
	DO NOTE ... NOT IMPLEMENTED YET: IF FORCED(LOC) SOMETHING
	DO NOTE ... NOT IMPLEMENTED YET: IF LOC=33 AND NOT CLOSNG, %25 ARBITRARY MESSAGE 8 "PLUGH"
	DO (201) NEXT
	PLEASE STOP

	PLEASE PRINT OUT DESCRIPTIONS OF OBJECTS AT THIS LOCATION.  IF NOT
	       CLOSING AND PROPERTY VALUE IS NEGATIVE, TALLY OFF ANOTHER
	       TREASURE.  RUG IS SPECIAL CASE; ONCE SEEN, ITS PROP IS 1 (DRAGON
	       ON IT) TILL DRAGON IS KILLED.  SIMILARLY FOR CHAIN; PROP IS
	       INITIALLY 1 (LOCKED TO BEAR).  THESE HACKS ARE BECAUSE PROP=0 IS
	       NEEDED TO GET FULL SCORE.
(201)	DO FORGET #1
	DO NOTE ... NOT IMPLEMENTED YET: IF DARK SKIP
	DO .1 <- ,61001SUB",61000SUB#1"#1 DO NOTE .1 <- ABB(LOC).
	DO (1020) NEXT
	DO ,61001SUB",61000SUB#1"#1 <- .1 DO NOTE ABB(LOC) <- .1
	DO NOTE ... NOT IMPLEMENTED YET: ITERATE OVER OBJECTS AT LOC
	DO (251) NEXT
	PLEASE STOP

	PLEASE CHECK IF THIS LOC IS ELIGIBLE FOR ANY HINTS.  IF BEEN HERE LONG
	       ENOUGH, BRANCH TO HELP SECTION (ON LATER PAGE).  HINTS ALL COME
	       BACK HERE EVENTUALLY TO FINISH THE LOOP.  IGNORE "HINTS" < 4
	       (SPECIAL STUFF, SEE DATABASE NOTES).
(251)	DO FORGET #1
	DO NOTE ... NOT IMPLEMENTED YET
	DO (301) NEXT
	PLEASE STOP

	PLEASE KICK THE RANDOM NUMBER GENERATOR JUST TO ADD VARIETY TO THE
	       CHASE.  ALSO, IF CLOSING TIME, CHECK FOR ANY OBJECTS BEING TOTED
	       WITH PROP < 0 AND SET THE PROP TO -1-PROP.  THIS WAY OBJECTS
	       WON'T BE DESCRIBED UNTIL THEY'VE BEEN PICKED UP AND PUT DOWN
	       SEPARATE FROM THEIR RESPECTIVE PILES.  DON'T TICK CLOCK1 UNLESS
	       WELL INTO CAVE (AND NOT AT Y2).
(301)	DO FORGET #1
	DO NOTE ... NOT IMPLEMENTED YET
	DO ,61000SUB#20 <- ,61000SUB#19 DO NOTE WZDARK <- DARK
	DO .1 <- ,61000SUB#4 DO NOTE .1 <- TURNS
	DO (1020) NEXT
	DO [TURNS] .1
	DO ,61000SUB#4 <- .1 DO NOTE TURNS <- .1
	DO (51001) NEXT DO NOTE CALL GETIN()
	DO (351) NEXT

	PLEASE EVERY INPUT, CHECK "FOOBAR" FLAG.  IF ZERO, NOTHING'S GOING ON.
	       IF POS, MAKE NEG.  IF NEG, HE SKIPPED A WORD, SO MAKE IT ZERO.
(351)	DO FORGET #1
	DO NOTE ... IF VERB = 'SAY' AND OBJECT != '' VERB = #0
	DO NOTE ... IF VERB = 'SAY', .2 <- .1 GOTO SAY
	DO NOTE ... IF(TALLY.EQ.0.AND.LOC.GE.15.AND.LOC.NE.33)CLOCK1=CLOCK1-1
	DO NOTE ... IF(CLOCK1.EQ.0) GOTO CAVE CLOSING FIRST WARNING
	DO NOTE ... IF(CLOCK1.LT.0)CLOCK2=CLOCK2-1
	DO NOTE ... IF(CLOCK2.EQ.0) GOTO CAVE CLOSING
	DO NOTE ... IF(PROP(LAMP).EQ.1)LIMIT=LIMIT-1
	DO NOTE ... IF(LIMIT.LE.30.AND.HERE(BATTERIES).AND.PROP(BATTERIES).EQ.0.AND.HERE(LAMP)) GOSUB AUTO-REPLACE BATTERIES
	DO NOTE ... IF(LIMIT.EQ.0) GOSUB LAMP GOES DEAD
	DO NOTE ... IF(LIMIT.LT.0.AND.LOC.LE.8) RSPEAK(185) GOTO GIVE UP
	DO NOTE ... IF(LIMIT.LE.30) GOSUB OTHER CASES OF LAMP DYING 
	DO NOTE ... IF(.1 = 'ENTER' AND .2 = 'STREA' OR .2 = 'WATER' AND LIQLOC(LOC).EQ.WATER) RSPEAK(70).
	DO NOTE ... IF(.1 = 'ENTER' AND .2 = 'STREA' OR .2 = 'WATER') RSPEAK(43).
	DO NOTE ... IF(.1 = 'ENTER' AND .2 != 0) .1 <- .2 I.E. GO WEST -> WEST
	DO NOTE ... IF(.1 = #64 'WEST')IWEST=IWEST+1
	DO NOTE ... IF(IWEST.EQ.10)RSPEAK(17) (IF YOU PREFER, SIMPLY TYPE W RATHER THAN WEST.)
	DO NOTE ... IF(.1 = 0) SORRY, I DON'T KNOW THE WORD ..., GOTO (251).
	DO NOTE ... VOCAB TAG IS MOTION VERB, GOTO FIGURE OUT THE NEW LOCATION
	DO NOTE ... VOCAB TAG IS OBJECT, GOTO ANALYSE AN OBJECT WORD
	DO NOTE ... VOCAB TAG IS ACTION VERB, GOTO ANALYSE A VERB
	DO NOTE ... VOCAB TAG IS #4, RSPEAK(VOCAB VALUE), VERB=0, OLD_OBJ=OBJ, OBJ=0
	DO (101) NEXT	

	PLEASE COMPUTE .3 <- .1 MOD .2
(40001)	DO STASH .1 + .2 + .4
	DO .4 <- .1
	DO (1040) NEXT
	DO .1 <- .3
	DO (1030) NEXT
	DO .1 <- .4
	DO .2 <- .3
	DO (1010) NEXT
	DO RETRIEVE .1 + .2 + .4
	DO RESUME #1

	PLEASE INITIALIZE INPUT/OUTPUT
	PLEASE CHOOSE PUKE BINARY I/O OR C-INTERCAL BINARY I/O
		BY ABSTAINING FROM (50000) FOR PUKE BINARY I/O OR
		REINSTATING (50000) FOR C-INTERCAL BINARY I/O
(50000)	DON'T (50001) NEXT
	DO ,50121 <- #2
	DO REINSTATE (50011)
	DO ABSTAIN FROM (50012)
	DO REINSTATE (50021)
	DO ABSTAIN FROM (50022)
	DO (50001) NEXT
	PLEASE STOP
(50001)	DO RESUME #2
	PLEASE STOP

	PLEASE READ OUT IN ONE BYTE FROM .1
	PLEASE RETAIN .50211 BETWEEN INVOCATIONS WHEN USING C-INTERCAL BINARY I/O
(50011)	DON'T (50111) NEXT
(50012) DO (50211) NEXT
	DO RESUME #1
	PLEASE STOP

	PLEASE WRITE IN ONE BYTE INTO .1
	PLEASE RETAIN ,50121 BETWEEN INVOCATIONS WHEN USING PUKE BINARY I/O
	PLEASE RETAIN .50221 BETWEEN INVOCATIONS WHEN USING C-INTERCAL BINARY I/O
(50021)	DON'T (50121) NEXT
(50022)	DO (50221) NEXT
	DO RESUME #1
	PLEASE STOP

	PLEASE READ OUT ONE BYTE FROM .1 USING PUKE BINARY I/O
(50111)	DO STASH ,8
	DO ,8 <- #8
	DO ,8SUB#1 <- .1~#1
	DO ,8SUB#2 <- !1~#2'¢#0
	DO ,8SUB#3 <- !1~#4'¢!1~#4'
	DO ,8SUB#4 <- #0¢"!1~#8'¢#0"
	DO ,8SUB#5 <- #0¢"!1~#16'¢!1~#16'"
	DO ,8SUB#6 <- !1~#32'¢"!1~#32'¢#0"
	DO ,8SUB#7 <- !1~#64'¢"!1~#64'¢!1~#64'"
	DO ,8SUB#8 <- "!1~#128'¢#0"¢#0
	DO READ OUT ,8
	DO RETRIEVE ,8
	DO RESUME #1
	PLEASE STOP

	PLEASE WRITE IN ONE BYTE TO .1 USING PUKE BINARY I/O
	PLEASE RETAIN ,50121 BETWEEN INVOCATIONS
(50121)	DO STASH .2 + .3 + .50120 + .50121
	DO .50120 <- #1
	DO .50121 <- #0
	DO (50122) NEXT
	PLEASE STOP
(50122)	DO FORGET #1
	DO (50123) NEXT
	DO (50130) NEXT
	PLEASE STOP
(50123)	DO (50124) NEXT
	DO .1 <- .50121
	DO RETRIEVE .2 + .3 + .50120 + .50121
	DO RESUME #2
	PLEASE STOP
(50124)	DO RESUME "∀'"#255~!50120~#255'"~#1'¢#1"~#3
	PLEASE STOP
(50130)	DO FORGET #1
	DO (50131) NEXT
	DO .1 <- ,50121SUB#1
	DO .2 <- #1
	DO (1010) NEXT
	DO ,50121SUB#1 <- .3
	DO .50120 <- !50120¢#0'~'#32767¢#1'
	DO (50122) NEXT
	PLEASE STOP
(50131)	DO (50132) NEXT
	DO FORGET #1
	DO (50140) NEXT
	PLEASE STOP
(50132)	DO RESUME "∀'"#65535~,50121SUB#1"~#1'¢#1"~#3
	PLEASE STOP
(50140)	DO FORGET #1
	DO (50141) NEXT
	DO .1 <- ,50121SUB#2
	DO .2 <- #1
	DO (1010) NEXT
	DO ,50121SUB#2 <- .3
	DO .1 <- .50120
	DO .2 <- .50121
	DO (1000) NEXT
	DO .50120 <- !50120¢#0'~'#32767¢#1'
	DO .50121 <- .3
	DO (50122) NEXT
	PLEASE STOP
(50141)	DO (50142) NEXT
	DO FORGET #1
	DO (50150) NEXT
	PLEASE STOP
(50142)	DO RESUME "∀'"#65535~,50121SUB#2"~#1'¢#1"~#3
	PLEASE STOP
(50150)	DO FORGET #1
	DO STASH ,50150
	DO ,50150 <- #1
	DO WRITE IN ,50150
	DO ,50121SUB#1 <- ,50150SUB#1
	DO ,50121SUB#2 <- #0
	DO RETRIEVE ,50150
	DO (50153) NEXT
	DO (50151) NEXT
	DO (50122) NEXT
	PLEASE STOP
(50151)	DO (50152) NEXT
	DO .1 <- .50121
	DO RETRIEVE .2 + .3 + .50120 + .50121
	DO RESUME #2
	PLEASE STOP
(50152)	DO RESUME '∀"'"#65535¢#65535"~"',50121SUB#1'¢',50121SUB#2'"'~#1"¢#1'~#3
	PLEASE STOP
(50153)	DO (50152) NEXT
	DO WRITE IN ,50121
	DO RESUME #1
	PLEASE STOP

	PLEASE READ OUT ONE BYTE FROM .1 USING C-INTERCAL BINARY I/O
	PLEASE RETAIN .50211 BETWEEN INVOCATIONS
(50211)	DO STASH .1 + .2 + .3 + ,50211
	DO ,50211 <- #1
	DO .2 <- '"!1~#1'¢!1~#16'"¢"!1~#4'¢!1~#64'"'¢'"!1~#2'¢!1~#32'"¢"!1~#8'¢!1~#128'"'
	DO .1 <- .50211
	DO .50211 <- .2
	DO (1010) NEXT
	DO ,50211SUB#1 <- .3~#255
	DO READ OUT ,50211
	DO RETRIEVE .1 + .2 + .3 + ,50211
	DO RESUME #1
	PLEASE STOP

	PLEASE WRITE IN ONE BYTE TO .1 USING C-INTERCAL BINARY I/O
	PLEASE RETAIN .50221 BETWEEN INVOCATIONS
(50221) DO STASH .2 + .3 + ,50221
	DO ,50221 <- #1
	DO WRITE IN ,50221
	DO (50222) NEXT
	DO (50224) NEXT
	PLEASE STOP
(50222)	DO (50223) NEXT
	DO .1 <- #0
	DO RETRIEVE .2 + .3 + ,50221
	DO RESUME #2
	PLEASE STOP
(50223)	DO RESUME '∀"'#511~"'∀#256¢,50221SUB#1'~'#0¢#511'"'~#1"¢#1'~#3
	PLEASE STOP
(50224)	DO .1 <- ,50221SUB#1
	DO .2 <- .50221
	DO (1000) NEXT
	DO .1 <- .3~#255
	DO .50221 <- .1
	DO RETRIEVE .2 + .3 + ,50221
	DO RESUME #2
	PLEASE STOP

	PLEASE DO LINE INPUT AND TOKENIZATION
	PLEASE USE (51000) FOR INITIALIZATION ENTRY POINT
	PLEASE USE (51001) FOR MAIN ENTRY POINT
	PLEASE NOTE ON RESUMPTION FROM (51001):
	PLEASE NOTE   .1 IS THE VOCABULARY TABLE INDEX OF THE FIRST WORD OR #0
	PLEASE NOTE   .2 IS THE VOCABULARY TABLE INDEX OF THE SECOND WORD OR #0
	PLEASE NOTE
	PLEASE NOTE ,51001 HOLDS THE INPUT
	PLEASE NOTE ,51001 SUB #1 .C CONTAINS UP TO #15 CHARACTERS OF THE
	PLEASE NOTE   FIRST WORD WITH #0 INDICATING THE END OF WORD
	PLEASE NOTE ,51001 SUB #2 .C CONTAINS UP TO #15 CHARACTERS OF THE
	PLEASE NOTE   SECOND WORD WITH #0 INDICATING THE END OF WORD
	PLEASE NOTE ,51001 SUB #3 #1 THE INDEX OF THE FIRST WORD'S ENTRY
	PLEASE NOTE   IN THE VOCABULARY TABLE ,60004 OR #0 IF NONE
	PLEASE NOTE ,51001 SUB #3 #2 THE INDEX OF THE SECOND WORD'S ENTRY
	PLEASE NOTE   IN THE VOCABULARY TABLE ,60004 OR #0 IF NONE
	PLEASE NOTE CHARACTERS BEYOND THE 15TH CHARACTER OF THE FIRST WORD,
	PLEASE NOTE   CHARACTERS BEYOND THE 15TH CHARACTER OF THE SECOND WORD,
	PLEASE NOTE   AND ALL CHARACTERS AFTER THE SECOND WORD ARE DISCARDED
	PLEASE NOTE   UNTIL THE END OF LINE
	PLEASE NOTE ONLY THE FIRST FIVE CHARACTERS OF THE FIRST AND SECOND
	PLEASE NOTE   WORDS ARE USED TO LOOK UP THE VOCABULARY TABLE INDEX
(51000)	DO ,51001 <- #3 BY #16
	DO RESUME #1
	PLEASE STOP
(51001)	DO ,51001SUB#1#1 <- #0  DO ,51001SUB#2#1 <- #0  DO ,51001SUB#3#1 <- #0
	DO ,51001SUB#1#2 <- #0  DO ,51001SUB#2#2 <- #0  DO ,51001SUB#3#2 <- #0
	DO ,51001SUB#1#3 <- #0  DO ,51001SUB#2#3 <- #0  DO ,51001SUB#3#3 <- #0
	DO ,51001SUB#1#4 <- #0  DO ,51001SUB#2#4 <- #0  DO ,51001SUB#3#4 <- #0
	DO ,51001SUB#1#5 <- #0  DO ,51001SUB#2#5 <- #0  DO ,51001SUB#3#5 <- #0
	DO ,51001SUB#1#6 <- #0  DO ,51001SUB#2#6 <- #0  DO ,51001SUB#3#6 <- #0
	DO ,51001SUB#1#7 <- #0  DO ,51001SUB#2#7 <- #0  DO ,51001SUB#3#7 <- #0
	DO ,51001SUB#1#8 <- #0  DO ,51001SUB#2#8 <- #0  DO ,51001SUB#3#8 <- #0
	DO ,51001SUB#1#9 <- #0  DO ,51001SUB#2#9 <- #0  DO ,51001SUB#3#9 <- #0
	DO ,51001SUB#1#10 <- #0 DO ,51001SUB#2#10 <- #0 DO ,51001SUB#3#10 <- #0
	DO ,51001SUB#1#11 <- #0 DO ,51001SUB#2#11 <- #0 DO ,51001SUB#3#11 <- #0
	DO ,51001SUB#1#12 <- #0 DO ,51001SUB#2#12 <- #0 DO ,51001SUB#3#12 <- #0
	DO ,51001SUB#1#13 <- #0 DO ,51001SUB#2#13 <- #0 DO ,51001SUB#3#13 <- #0
	DO ,51001SUB#1#14 <- #0 DO ,51001SUB#2#14 <- #0 DO ,51001SUB#3#14 <- #0
	DO ,51001SUB#1#15 <- #0 DO ,51001SUB#2#15 <- #0 DO ,51001SUB#3#15 <- #0
	DO ,51001SUB#1#16 <- #0 DO ,51001SUB#2#16 <- #0 DO ,51001SUB#3#16 <- #0
	DO ,51001SUB#3#4 <- #1
	DO ,51001SUB#3#5 <- #7
	DO ,51001SUB#3#6 <- #8
	DO (50021) NEXT
	DO (51002) NEXT
	PLEASE SKIP LEADING SPACES, FIRST CHECKING FOR END OF LINE
	PLEASE NOTE   ,51001SUB#3#3 IS CURRENT CHARACTER INDEX
	PLEASE NOTE   ,51001SUB#3#4 IS CURRENT WORD INDEX (#1 OR #2)
	PLEASE NOTE   ,51001SUB#3#5 IS CURRENT WORD INDEX FOR FIRST
	PLEASE NOTE      THREE CHARACTERS (#7 OR #9)
	PLEASE NOTE   ,51001SUB#3#6 IS CURRENT WORD INDEX FOR LAST
	PLEASE NOTE      TWO CHARACTERS (#8 OR #10)
	PLEASE NOTE   ,51001SUB#3#7 IS FIRST THREE CHARACTERS OF FIRST WORD
	PLEASE NOTE     AT FIVE BITS PER CHARACTER
	PLEASE NOTE   ,51001SUB#3#8 IS LAST TWO CHARACTERS OF FIRST WORD
	PLEASE NOTE   ,51001SUB#3#9 IS FIRST THREE CHARACTERS OF SECOND WORD
	PLEASE NOTE     AT FIVE BITS PER CHARACTER
	PLEASE NOTE   ,51001SUB#3#10 IS LAST TWO CHARACTERS OF SECOND WORD
(51002)	DO FORGET #1
	DO NOTE CHECK FOR END OF LINE
	DO (51003) NEXT
	DO NOTE NOT AT END OF LINE, CHECK FOR SPACE
	DO (51005) NEXT
	DO NOTE NOT SPACE, START WRITING IN FIRST OR SECOND WORD
	DO ,51001SUB#3#3 <- #1
	DO (51011) NEXT
	PLEASE STOP
(51003)	DO (51004) NEXT
	DO FORGET #1
	DO NOTE AT END OF LINE
	DO (51081) NEXT
(51004)	DO RESUME '∀"'#255~"'∀#10¢.1'~'#0¢#255'"'~#1"¢#1'~#3
	PLEASE STOP
(51005)	DO (51006) NEXT
	DO NOTE AT SPACE
	DO FORGET #1
	DO (50021) NEXT
	DO (51002) NEXT
	PLEASE STOP
(51006)	DO RESUME '∀"'#255~"'∀#32¢.1'~'#0¢#255'"'~#1"¢#1'~#3
	PLEASE STOP
	PLEASE WRITE IN CURRENT CHARACTER INTO CURRENT WORD
(51011)	DO FORGET #1
	DO ,51001SUB',51001SUB#3#4'',51001SUB#3#3' <- .1
	DO NOTE WRITE INTO FIVE BITS
	DO (51031) NEXT
	DO .1 <- ,51001SUB#3#3
	DO (1020) NEXT
	DO (51012) NEXT
	DO NOTE NOT AT 16TH CHARACTER, ADVANCE TO NEXT CHARACTER
	DO ,51001SUB#3#3 <- .1
	DO (50021) NEXT
	DO NOTE CHECK FOR END OF LINE
	DO (51003) NEXT
	DO NOTE NOT AT END OF LINE, CHECK FOR SPACE
	DO (51014) NEXT
	DO NOTE NOT SPACE, CONTINUE WRITING IN CURRENT WORD
	DO (51011) NEXT
	PLEASE STOP
(51012)	DO (51013) NEXT
	DO FORGET #1
	DO NOTE AT 16TH CHARACTER, DISCARD UNTIL SPACE OR END OF LINE
	DO (51018) NEXT
	PLEASE STOP
(51013)	DO RESUME '∀"'#255~"'∀#16¢.1'~'#0¢#255'"'~#1"¢#1'~#3
	PLEASE STOP
(51014)	DO (51006) NEXT
	DO FORGET #1
	DO NOTE AT END OF WORD, CHECK IF FIRST WORD OR SECOND WORD
	DO (51015) NEXT
	DO NOTE FINISHED SECOND WORD, DISCARD UNTIL END OF LINE
	DO (51017) NEXT
	PLEASE STOP
(51015)	DO (51016) NEXT
	DO FORGET #1
	DO NOTE FINISHED FIRST WORD, SKIP SPACES AND START SECOND WORD
	DO ,51001SUB#3#4 <- #2
	DO ,51001SUB#3#5 <- #9
	DO ,51001SUB#3#6 <- #10
	DO (51002) NEXT
	PLEASE STOP
(51016)	DO RESUME ,51001SUB#3#4
	PLEASE STOP
(51017)	DO FORGET #1
	DO NOTE DISCARD UNTIL END OF LINE
	DO (50021) NEXT
	DO (51003) NEXT
	DO (51017) NEXT
	PLEASE STOP
(51018)	DO FORGET #1
	DO NOTE DISCARD UNTIL SPACE OR END OF LINE
	DO (50021) NEXT
	DO (51003) NEXT
	DO (51005) NEXT
	DO (51018) NEXT
	PLEASE STOP

	PLEASE CONVERT .1 TO FIVE BIT REPRESENTATION WHERE 2 IS MAPPED TO #29
	       AND " IS MAPPED TO #30
(51021)	DO (51022) NEXT
	DO NOTE NOT 2
	DO (51024) NEXT
	DO NOTE NOT "
	DO .1 <- .1~#31
	DO RESUME #1
	PLEASE STOP
(51022)	DO (51023) NEXT
	DO .1 <- #29
	DO RESUME #2
	PLEASE STOP
(51023)	DO RESUME '∀"'#255~"'∀#50¢.1'~'#0¢#255'"'~#1"¢#1'~#3
	PLEASE STOP
(51024)	DO (51025) NEXT
	DO .1 <- #30
	DO RESUME #2
	PLEASE STOP
(51025)	DO RESUME '∀"'#255~"'∀#34¢.1'~'#0¢#255'"'~#1"¢#1'~#3
	PLEASE STOP

	PLEASE ADD FIVE BIT REPRESENTATION TO ONE OF
	       ,51001SUB#3#7 ,51001SUB#3#8 ,51001SUB#3#9 ,51001SUB#3#10
	       AFTER MULTIPLYING BY ONE OF 1, 32, OR 1024
(51031)	DO STASH .1 + .2 + .3
	DO (51021) NEXT
	DO (51032) NEXT
	DO NOTE ,51001SUB#3#3, CURRENT CHARACTER INDEX, IS NOT 1
	DO (51034) NEXT
	DO NOTE ,51001SUB#3#3, CURRENT CHARACTER INDEX, IS NOT 2
	DO (51036) NEXT
	DO NOTE ,51001SUB#3#3, CURRENT CHARACTER INDEX, IS NOT 3
	DO (51038) NEXT
	DO NOTE ,51001SUB#3#3, CURRENT CHARACTER INDEX, IS NOT 4
	DO (51040) NEXT
	DO NOTE ,51001SUB#3#3, CURRENT CHARACTER INDEX, IS NOT 5
	DO (51042) NEXT
	PLEASE STOP
(51032)	DO (51033) NEXT
	DO NOTE ,51001SUB#3#3, CURRENT CHARACTER INDEX, IS 1
	DO FORGET #1
	DO ,51001SUB#3",51001SUB#3#5" <- .1
	DO (51042) NEXT
	PLEASE STOP
(51033)	DO RESUME '∀"'#255~"'∀#1¢",51001SUB#3#3"'~'#0¢#255'"'~#1"¢#1'~#3
	PLEASE STOP
(51034)	DO (51035) NEXT
	DO NOTE ,51001SUB#3#3, CURRENT CHARACTER INDEX, IS 2
	DO FORGET #1
	DO .2 <- #32
	DO (1030) NEXT
	DO .1 <- .3
	DO .2 <- ,51001SUB#3",51001SUB#3#5"
	DO (1000) NEXT
	DO ,51001SUB#3",51001SUB#3#5" <- .3
	DO (51042) NEXT
	PLEASE STOP
(51035)	DO RESUME '∀"'#255~"'∀#2¢",51001SUB#3#3"'~'#0¢#255'"'~#1"¢#1'~#3
	PLEASE STOP
(51036)	DO (51037) NEXT
	DO NOTE ,51001SUB#3#3, CURRENT CHARACTER INDEX, IS 3
	DO FORGET #1
	DO .2 <- #1024
	DO (1030) NEXT
	DO .1 <- .3
	DO .2 <- ,51001SUB#3",51001SUB#3#5"
	DO (1000) NEXT
	DO ,51001SUB#3",51001SUB#3#5" <- .3
	DO (51042) NEXT
	PLEASE STOP
(51037)	DO RESUME '∀"'#255~"'∀#3¢",51001SUB#3#3"'~'#0¢#255'"'~#1"¢#1'~#3
	PLEASE STOP
(51038)	DO (51039) NEXT
	DO NOTE ,51001SUB#3#3, CURRENT CHARACTER INDEX, IS 4
	DO FORGET #1
	DO ,51001SUB#3",51001SUB#3#6" <- .1
	DO (51042) NEXT
	PLEASE STOP
(51039)	DO RESUME '∀"'#255~"'∀#4¢",51001SUB#3#3"'~'#0¢#255'"'~#1"¢#1'~#3
	PLEASE STOP
(51040)	DO (51041) NEXT
	DO NOTE ,51001SUB#3#3, CURRENT CHARACTER INDEX, IS 5
	DO FORGET #1
	DO .2 <- #32
	DO (1030) NEXT
	DO .1 <- .3
	DO .2 <- ,51001SUB#3",51001SUB#3#6"
	DO (1000) NEXT
	DO ,51001SUB#3",51001SUB#3#6" <- .3
	DO (51042) NEXT
	PLEASE STOP
(51041)	DO RESUME '∀"'#255~"'∀#5¢",51001SUB#3#3"'~'#0¢#255'"'~#1"¢#1'~#3
	PLEASE STOP
(51042)	DO RETRIEVE .1 + .2 + .3
	DO RESUME #2
	PLEASE STOP

	PLEASE LOOK UP WORDS IN VOCABULARY TABLE, THEN
	       RETRIEVE STASHED VARS AND RESUME
(51081)	DO FORGET #1
	DO .1 <- #1
	DO ,51001SUB#3#1 <- #0
	DO ,51001SUB#3#2 <- #0
	DO (51082) NEXT
	PLEASE STOP
(51082)	DO FORGET #1
	DO (51085) NEXT
	DO (1020) NEXT
	DO (51083) NEXT
	DO (51082) NEXT
	PLEASE STOP
(51083)	DO (51084) NEXT
	DO FORGET #1
	DO .1 <- #1
	DO (51089) NEXT
	PLEASE STOP
(51084)	DO RESUME '∀"'#511~"'∀#297¢.1'~'#0¢#511'"'~#1"¢#1'~#3
	PLEASE STOP
(51085)	DO (51086) NEXT
	DO FORGET #1
	DO (51087) NEXT
	DO (1020) NEXT
	DO (51083) NEXT
	DO (51082) NEXT
	PLEASE STOP
(51086)	DO RESUME '∀"'#65535~"'∀",51001SUB#3#7"¢",60004SUB.1#1"'~'#0¢#65535'"'~#1"¢#1'~#3
	PLEASE STOP	
(51087)	DO (51088) NEXT
	DO FORGET #1
	DO ,51001SUB#3#1 <- .1
	DO .1 <- #1
	DO (51089) NEXT
	PLEASE STOP
(51088)	DO RESUME '∀"'#65535~"'∀",51001SUB#3#8"¢",60004SUB.1#2"'~'#0¢#65535'"'~#1"¢#1'~#3
	PLEASE STOP	
(51089)	DO FORGET #1
	DO (51091) NEXT
	DO (1020) NEXT
	DO (51090) NEXT
	DO (51089) NEXT
	PLEASE STOP
(51090)	DO (51084) NEXT
	DO FORGET #1
	DO (51095) NEXT
	PLEASE STOP
(51091)	DO (51092) NEXT
	DO FORGET #1
	DO (51093) NEXT
	DO (1020) NEXT
	DO (51090) NEXT
	DO (51089) NEXT
	PLEASE STOP
(51092)	DO RESUME '∀"'#65535~"'∀",51001SUB#3#9"¢",60004SUB.1#1"'~'#0¢#65535'"'~#1"¢#1'~#3
	PLEASE STOP
(51093)	DO (51094) NEXT
	DO FORGET #1
	DO ,51001SUB#3#2 <- .1
	DO (51095) NEXT
	PLEASE STOP
(51094)	DO RESUME '∀"'#65535~"'∀",51001SUB#3#10"¢",60004SUB.1#2"'~'#0¢#65535'"'~#1"¢#1'~#3
	PLEASE STOP
(51095)	DO FORGET #1
	DO .1 <- ,51001SUB#3#1
	DO .2 <- ,51001SUB#3#2
	DO RESUME #1
	PLEASE STOP

	PLEASE READ OUT LONG FORM DESCRIPTION
	       .1 IS OBJECT INDEX
(55101)	DO STASH .1 + .2 + .3
	DO .2 <- .1
	DO .3 <- #1
	DO (55102) NEXT
	PLEASE STOP
(55102)	DO FORGET #1
	DO .1 <- ,60001SUB.2.3
	DO (55103) NEXT
	DO (50011) NEXT
	DO .1 <- .3
	DO (1020) NEXT
	DO .3 <- .1
	DO (55102) NEXT
	PLEASE STOP
(55103)	DO (55104) NEXT
	DO RETRIEVE .1 + .2 + .3
	DO RESUME #2
	PLEASE STOP
(55104)	DO RESUME "∀'#255~.1'¢#1"~#3
	PLEASE STOP

	PLEASE READ OUT LOCATION SHORT FORM DESCRIPTION
	       .1 IS OBJECT INDEX
(55201)	DO (55202) NEXT
	DO NOTE HAVE SHORT FORM DESCRIPTION
	DO STASH .1 + .2 + .3
	DO .2 <- .1
	DO .3 <- #1
	DO (55204) NEXT
	PLEASE STOP
(55202)	DO (55203) NEXT
	DO NOTE NO SHORT FORM DESCRIPTION, READ OUT LONG FORM
	DO (55101) NEXT
	DO RESUME #2
	PLEASE STOP
(55203)	DO RESUME "∀'#255~,60002SUB.1#1'¢#1"~#3
	PLEASE STOP
(55204)	DO FORGET #1
	DO .1 <- ,60002SUB.2.3
	DO (55103) NEXT
	DO (50011) NEXT
	DO .1 <- .3
	DO (1020) NEXT
	DO .3 <- .1
	DO (55204) NEXT
	PLEASE STOP

	PLEASE READ OUT OBJECT DESCRIPTION
	       .1 IS OBJECT INDEX
	       .2 IS MESSAGE INDEX
(55501)	DO STASH .1 + .2 + .3 + .4
	DO .3 <- .1
	DO .4 <- #1
	DO (55502) NEXT
	PLEASE STOP
(55502)	DO FORGET #1
	DO .1 <- ,60005SUB.3.2.4
	DO (55503) NEXT
	DO (50011) NEXT
	DO .1 <- .4
	DO (1020) NEXT
	DO .4 <- .1
	DO (55502) NEXT
	PLEASE STOP
(55503)	DO (55104) NEXT
	DO RETRIEVE .1 + .2 + .3 + .4
	DO RESUME #2
	PLEASE STOP

	PLEASE READ OUT ARBITRARY MESSAGE
	       .1 IS MESSAGE INDEX
(55601)	DO STASH .1 + .2 + .3
	DO .2 <- .1
	DO .3 <- #1
	DO (55602) NEXT
	PLEASE STOP
(55602)	DO FORGET #1
	DO .1 <- ,60006SUB.2.3
	DO (55103) NEXT
	DO (50011) NEXT
	DO .1 <- .3
	DO (1020) NEXT
	DO .3 <- .1
	DO (55602) NEXT
	PLEASE STOP

	PLEASE READ OUT CLASSIFICATION MESSAGE
	       .1 HAS THE SCORE
(56001)	DO STASH .1 + .2 + .3 + .4 + .5
	DO .4 <- .1
	DO .5 <- #1
	DO (56002) NEXT
	PLEASE STOP
(56002)	DO FORGET #1
	DO .1 <- ,60010SUB.5#1
	DO .2 <- .4
	DO (1010) NEXT
	DO (56003) NEXT
	DO .1 <- .5
	DO (1020) NEXT
	DO .5 <- .1
	DO (56002) NEXT
	PLEASE STOP
(56003)	DO (56004) NEXT
	DO FORGET #1
	DO .2 <- #2
	DO (56005) NEXT
	PLEASE STOP
(56004)	DO RESUME "∀'.3~#32768'¢#1"~#3
	PLEASE STOP
(56005)	DO FORGET #1
	DO .1 <- ,60010SUB.5.2
	DO (56006) NEXT
	DO (50011) NEXT
	DO .1 <- .2
	DO (1020) NEXT
	DO .2 <- .1
	DO (56005) NEXT
	PLEASE STOP
(56006)	DO (56007) NEXT
	DO RETRIEVE .1 + .2 + .3 + .4 + .5
	DO RESUME #2
	PLEASE STOP
(56007)	DO RESUME "∀'"#255~.1"~#1'¢#1"~#3
	PLEASE STOP

	PLEASE READ OUT MAGIC MESSAGE
	       .1 IS MAGIC MESSAGE INDEX INDEX
(56201)	DO STASH .1 + .2 + .3
	DO .2 <- .1
	DO .3 <- #1
	DO (56202) NEXT
	PLEASE STOP
(56202)	DO FORGET #1
	DO .1 <- ,60012SUB.2.3
	DO (55103) NEXT
	DO (50011) NEXT
	DO .1 <- .3
	DO (1020) NEXT
	DO .3 <- .1
	DO (56202) NEXT
	PLEASE STOP

	PLEASE READ OUT CUSTOM MESSAGE IN STARTING FROM ,60006SUB#13#2
(57001)	DO STASH .1 + .2 + .3
	DO .2 <- #13
	DO .3 <- #2
	DO (55602) NEXT
	PLEASE STOP

	PLEASE WRITE IN YES OR NO
	       .1 IS #1 IF YES
	       .1 IS #2 IF NO
(57101)	DO STASH .2 + .3
	DO (57102) NEXT
	PLEASE STOP
(57102)	DO FORGET #1
	DO (51001) NEXT
	DO .1 <- ,51001SUB#3#8
	DO .2 <- #0
	DO (57103) NEXT
	DO (57106) NEXT
	PLEASE STOP
(57103)	DO (57104) NEXT
	DO FORGET #1
	DO .1 <- ,51001SUB#3#7
	DO .2 <- #19641
	DO .3 <- #1
	DO (57105) NEXT
	DO .2 <- #25
	DO (57105) NEXT
	DO .3 <- #2
	DO .2 <- #494
	DO (57105) NEXT
	DO .2 <- #14
	DO (57105) NEXT
	DO (57106) NEXT
	PLEASE STOP
(57104)	DO RESUME "∀'"'#0¢#65535'~'∀.1¢.2'"~#1'¢#1"~#3
	PLEASE STOP
(57105)	DO (57104) NEXT
	DO .1 <- .3
	DO RETRIEVE .2 + .3
	DO RESUME #2
	PLEASE STOP
(57106)	DO FORGET #1
	DO,60006SUB#13#2<-#80  DO,60006SUB#13#3<-#76  DO,60006SUB#13#4<-#69
	DO,60006SUB#13#5<-#65  DO,60006SUB#13#6<-#83  DO,60006SUB#13#7<-#69
	DO,60006SUB#13#8<-#32  DO,60006SUB#13#9<-#65  DO,60006SUB#13#10<-#78
	DO,60006SUB#13#11<-#83 DO,60006SUB#13#12<-#87 DO,60006SUB#13#13<-#69
	DO,60006SUB#13#14<-#82 DO,60006SUB#13#15<-#32 DO,60006SUB#13#16<-#84
	DO,60006SUB#13#17<-#72 DO,60006SUB#13#18<-#69 DO,60006SUB#13#19<-#32
	DO,60006SUB#13#20<-#81 DO,60006SUB#13#21<-#85 DO,60006SUB#13#22<-#69
	DO,60006SUB#13#23<-#83 DO,60006SUB#13#24<-#84 DO,60006SUB#13#25<-#73
	DO,60006SUB#13#26<-#79 DO,60006SUB#13#27<-#78 DO,60006SUB#13#28<-#46
	DO,60006SUB#13#29<-#10 DO,60006SUB#13#30<-#0
	DO (57001) NEXT
	DO (57102) NEXT

	PLEASE INITIALIZE DATABASE
	PLEASE NOTE ,60001 CONTAINS LONG FORM DESCRIPTIONS
	PLEASE NOTE  FIRST SUBSCRIPT IS LOCATION INDEX
	PLEASE NOTE  SECOND SUBSCRIPT IS CHARACTER INDEX
	PLEASE NOTE  ZERO TERMINATES DESCRIPTION
	PLEASE NOTE
	PLEASE NOTE ,60002 CONTAINS SHORT FORM DESCRIPTIONS
	PLEASE NOTE  FIRST SUBSCRIPT IS LOCATION INDEX
	PLEASE NOTE  SECOND SUBSCRIPT IS CHARACTER INDEX
	PLEASE NOTE    ZERO TERMINATES DESCRIPTION
	PLEASE NOTE    EMPTY MEANS USE LONG FORM DESCRIPTION
	PLEASE NOTE
	PLEASE NOTE ,60013 CONTAINS TRAVEL TABLE START INDEX INTO ,60004
	PLEASE NOTE   SUBSCRIPT IS LOCATION INDEX
	PLEASE NOTE ,60003 CONTAINS TRAVEL TABLE
	PLEASE NOTE   SUB #1 IS LOCATION INDEX
	PLEASE NOTE   SUB #2
	PLEASE NOTE     UPPER 8 BITS IS TAG
	PLEASE NOTE     LOWER 8 BITS IS VALUE
	PLEASE NOTE     WHEN TAG IS #1 VALUE IS DESTINATION LOCATION
	PLEASE NOTE     WHEN TAG IS #2 VALUE IS USED BY SPECIAL CODE SECTION
	PLEASE NOTE     WHEN TAG IS #3 VALUE IS INDEX INTO ARBITRARY MESSAGES
	PLEASE NOTE       TABLE ,60006
	PLEASE NOTE   SUB #3
	PLEASE NOTE     UPPER 8 BITS IS TAG
	PLEASE NOTE     LOWER 8 BITS IS VALUE
	PLEASE NOTE     WHEN TAG IS #1, MOTION IS UNCONDITIONAL
	PLEASE NOTE     WHEN TAG IS #2, MOTION IS UNCONDITIONAL
	PLEASE NOTE       BUT FORBIDDEN TO DWARVES
	PLEASE NOTE     WHEN TAG IS #3 VALUE IS THE % PROBABILITY OF MOTION
	PLEASE NOTE     WHEN TAG IS #4 VALUE IS INDEX OF OBJECT THAT MUST BE
	PLEASE NOTE       CARRIED FOR MOTION
	PLEASE NOTE     WHEN TAG IS #5 VALUE IS INDEX OF OBJECT THAT MUST BE
	PLEASE NOTE       CARRIED OR IN THE SAME ROOM FOR MOTION
	PLEASE NOTE     WHEN TAG IS #6 VALUE IS INDEX OF PROPERTY THAT MUST
	PLEASE NOTE       *NOT* BE #0 FOR MOTION
	PLEASE NOTE     WHEN TAG IS #7 VALUE IS INDEX OF PROPERTY THAT MUST
	PLEASE NOTE       *NOT* BE #1 FOR MOTION
	PLEASE NOTE     WHEN TAG IS #8 VALUE IS INDEX OF PROPERTY THAT MUST
	PLEASE NOTE       *NOT* BE #2 FOR MOTION
	PLEASE NOTE     WHEN TAG IS #9 VALUE IS INDEX OF PROPERTY THAT MUST
	PLEASE NOTE       *NOT* BE #3 FOR MOTION
	PLEASE NOTE     WHEN TAG IS #10 VALUE IS INDEX OF PROPERTY THAT MUST
	PLEASE NOTE       *NOT* BE #4 FOR MOTION
	PLEASE NOTE   SUB #4 OR MORE IS LIST OF MOTION VERBS
	PLEASE NOTE     ZERO IS END OF LIST
	PLEASE NOTE     ONE IS SPECIAL ACTION TAKEN ON ENTERING LOCATION (STILL
	PLEASE NOTE       SUBJECT TO CONDITIONS OF ELEMENT #2)
	PLEASE NOTE
	PLEASE NOTE ,60004 CONTAINS VOCABULARY TABLE
	PLEASE NOTE   FIRST SUBSCRIPT IS THE TABLE INDEX
	PLEASE NOTE   THE ONLY NON-ALPHABETICAL CHARACTERS ARE 2, ", AND ?
	PLEASE NOTE     2 IS MAPPED TO #29
	PLEASE NOTE     " IS MAPPED TO #30
	PLEASE NOTE     ? IS MAPPED TO #31
	PLEASE NOTE   USE THE LOWER 5 BITS OF EACH CHARACTER
	PLEASE NOTE   SUB.I#1 IS FIRST THREE CHARACTERS
	PLEASE NOTE     FIRST CHARACTER IS BITS 1-5
	PLEASE NOTE     SECOND CHARACTER IS BITS 6-10
	PLEASE NOTE     THIRD CHARACTER IS BITS 11-15
	PLEASE NOTE   SUB.I#2 IS FIRST FOURTH AND FIFTH CHARACTERS
	PLEASE NOTE     FOURTH CHARACTER IS BITS 1-5
	PLEASE NOTE     FIFTH CHARACTER IS BITS 6-10
	PLEASE NOTE   SUB.I#3
	PLEASE NOTE     UPPER 8 BITS IS TAG
	PLEASE NOTE     LOWER 8 BITS IS VALUE
	PLEASE NOTE     WHEN TAG IS #1 VALUE IS MOTION VERB INDEX
	PLEASE NOTE     WHEN TAG IS #2 VALUE IS OBJECT INDEX
	PLEASE NOTE     WHEN TAG IS #3 VALUE IS ACTION VERB INDEX
	PLEASE NOTE     WHEN TAG IS #4 VALUE IS INDEX INTO ARBITRARY MESSAGES
	PLEASE NOTE       TABLE ,60006
	PLEASE NOTE   DO NOT KNOW YET WHAT DUPLICATE FEE FIE FOE FOO FUM MEANS
	PLEASE NOTE   DO NOT KNOW YET WHAT TO DO WITH DUPLICATE ROD PLANT TROLL
	PLEASE NOTE     BUT THEY MUST REFER THE ALTERNATE ROD, PLANT, AND TROLL
	PLEASE NOTE     OBJECTS
	PLEASE NOTE   INITIALLY CONSIDERED USING
	PLEASE NOTE     #32 BY #32 BY #32 BY #32 BY #32 TRIE FOR FAST LOOKUPS
	PLEASE NOTE     WITH #0 MAPPED TO #27 BUT THAT WASTES TOO MUCH MEMORY
	PLEASE NOTE
	PLEASE NOTE ,60005 CONTAINS OBJECT DESCRIPTION TABLE
	PLEASE NOTE   FIRST SUBSCRIPT IS OBJECT INDEX
	PLEASE NOTE   SECOND SUBSCRIPT IS OBJECT MESSAGE INDEX
	PLEASE NOTE     #1 IS INVENTORY MESSAGE
	PLEASE NOTE     #2 IS OBJECT DESCRIPTION WHEN OBJECT PROP VALUE IS #0
	PLEASE NOTE     #3 IS OBJECT DESCRIPTION WHEN OBJECT PROP VALUE IS #1
	PLEASE NOTE     #4 IS OBJECT DESCRIPTION WHEN OBJECT PROP VALUE IS #2
	PLEASE NOTE     #5 IS OBJECT DESCRIPTION WHEN OBJECT PROP VALUE IS #3
	PLEASE NOTE     #6 IS OBJECT DESCRIPTION WHEN OBJECT PROP VALUE IS #4
	PLEASE NOTE     #7 IS OBJECT DESCRIPTION WHEN OBJECT PROP VALUE IS #5
	PLEASE NOTE   THIRD SUBSCRIPT IS CHARACTER INDEX
	PLEASE NOTE     ZERO TERMINATES MESSAGE
	PLEASE NOTE
	PLEASE NOTE ,60006 CONTAINS ARBITRARY MESSAGES TABLE
	PLEASE NOTE   FIRST SUBSCRIPT IS ARBITRARY MESSAGE INDEX
	PLEASE NOTE   SECOND SUBSCRIPT IS CHARACTER INDEX
	PLEASE NOTE   ZERO TERMINATES MESSAGE
	PLEASE NOTE
	PLEASE NOTE ,60007 CONTAINS OBJECT LOCATION INDEX
	PLEASE NOTE   FIRST SUBSCRIPT IS OBJECT INDEX
	PLEASE NOTE   SECOND SUBSCRIPT IS MULTIPLE LOCATIONS INDEX
	PLEASE NOTE     #1 IS LOCATION INDEX, #0 MEANS NO LOCATION
	PLEASE NOTE	#2 IS SECOND LOCATION INDEX, NON-ZERO MEANS
	PLEASE NOTE	   OBJECT IS IMMOVABLE
	PLEASE NOTE
	PLEASE NOTE ,60008 CONTAINS ACTION VERB DEFAULTS
	PLEASE NOTE   SUBSCRIPT IS ACTION VERB INDEX
	PLEASE NOTE   VALUE IS ARBITRARY MESSAGE INDEX INTO ,60006
	PLEASE NOTE
	PLEASE NOTE ,60009 CONTAINS LOCATION FLAGS
	PLEASE NOTE   SUBSCRIPT IS LOCATION INDEX
	PLEASE NOTE   VALUE~#1 IS LIGHT FLAG
	PLEASE NOTE   VALUE~#2 IS #1 FOR OIL #0 FOR WATER IF VALUE~#4 IS #1
	PLEASE NOTE   VALUE~#4 IS LIQUID FLAG
	PLEASE NOTE   VALUE~#8 IS PIRATE OFF LIMITS UNLESS FOLLOWING PLAYER
	PLEASE NOTE   VALUE~#16 IS HINT FOR TRYING TO GET INTO CAVE
	PLEASE NOTE   VALUE~#32 IS HINT FOR TRYING TO CATCH BIRD
	PLEASE NOTE   VALUE~#64 IS HINT FOR TRYING TO DEAL WITH SNAKE
	PLEASE NOTE   VALUE~#128 IS HINT FOR LOST IN MAZE
	PLEASE NOTE   VALUE~#256 IS HINT FOR PONDERING DARK ROOM
	PLEASE NOTE   VALUE~#512 IS HINT FOR AT WITT'S END
	PLEASE NOTE
	PLEASE NOTE ,60010 CONTAINS CLASSIFICATION MESSAGES
	PLEASE NOTE   FIRST SUBSCRIPT CLASSIFICATION INDEX
	PLEASE NOTE   WHEN SECOND SUBSCRIPT IS #1
	PLEASE NOTE     VALUE IS MAXIMUM SCORE FOR THIS CLASSIFICATION
	PLEASE NOTE   SUBSEQUENT VALUES OF THE SECOND SUBSCRIPT ARE
	PLEASE NOTE     CHARACTERS IN THE CLASSIFICATION MESSAGE
	PLEASE NOTE     WHERE ZERO TERMINATES THE MESSAGE
	PLEASE NOTE
	PLEASE NOTE ,60011 CONTAINS HINT INFORMATION
	PLEASE NOTE   TO BE DESCRIBED LATER...
	PLEASE NOTE
	PLEASE NOTE ,60012 CONTAINS MAGIC MESSAGES
	PLEASE NOTE   FIRST SUBSCRIPT IS MAGIC MESSAGE INDEX
	PLEASE NOTE   SECOND SUBSCRIPT IS CHARACTER INDEX
	PLEASE NOTE   ZERO TERMINATES MESSAGE
(60000)	DO ,60001 <- #140 BY #1294
	DO ,60002 <- #140 BY #50
	DO ,60013 <- #140
	DO ,60003 <- #500 BY #10
	DO ,60004 <- #296 BY #3
	DO ,60005 <- #64 BY #7 BY #138
	DO ,60006 <- #201 BY #1381
	DO ,60007 <- #64 BY #2
	DO ,60008 <- #31
	DO ,60009 <- #140
	DO ,60010 <- #9 BY #68
	DO ,60011 <- #9 BY #4
	DO ,60012 <- #32 BY #320

	DO,60006SUB#13#2<-#73  DO,60006SUB#13#3<-#78  DO,60006SUB#13#4<-#73
	DO,60006SUB#13#5<-#84  DO,60006SUB#13#6<-#73  DO,60006SUB#13#7<-#65
	DO,60006SUB#13#8<-#76  DO,60006SUB#13#9<-#73  DO,60006SUB#13#10<-#83
	DO,60006SUB#13#11<-#73 DO,60006SUB#13#12<-#78 DO,60006SUB#13#13<-#71
	DO,60006SUB#13#14<-#46 DO,60006SUB#13#15<-#46 DO,60006SUB#13#16<-#46
	DO,60006SUB#13#17<-#10 DO,60006SUB#13#18<-#0
	DO (57001) NEXT

	DO (65000) NEXT

	DO,60006SUB#13#6<-#32  DO,60006SUB#13#7<-#68  DO,60006SUB#13#8<-#79
	DO,60006SUB#13#9<-#78  DO,60006SUB#13#10<-#69 DO,60006SUB#13#11<-#10
	DO,60006SUB#13#12<-#0
	DO (57001) NEXT
	DO RESUME #1

	PLEASE INITIALIZE STATE
	PLEASE NOTE FLAGS ARE #0 FOR FALSE AND #1 FOR TRUE
	PLEASE NOTE
	PLEASE NOTE ,61000SUB#1 IS LOC
	PLEASE NOTE ,61000SUB#2 IS NEWLOC
	PLEASE NOTE ,61000SUB#3 IS OLDLOC
	PLEASE NOTE ,61000SUB#4 IS TURNS TALLIES HOW MANY COMMANDS GIVEN
	PLEASE NOTE ,61000SUB#5 IS LIMIT LIFETIME OF LAMP
	PLEASE NOTE ,61000SUB#6 IS IWEST
	PLEASE NOTE ,61000SUB#7 IS KNFLOC
	PLEASE NOTE ,61000SUB#8 IS DETAIL
	PLEASE NOTE ,61000SUB#9 IS ABBNUM IS HOW OFTEN TO PRINT LONG DESCRIPTIONS
	PLEASE NOTE ,61000SUB#10 IS MAXDIE
	PLEASE NOTE ,61000SUB#11 IS NUMDIE
	PLEASE NOTE ,61000SUB#12 IS HOLDNG
	PLEASE NOTE ,61000SUB#13 IS DKILL
	PLEASE NOTE ,61000SUB#14 IS FOOBAR
	PLEASE NOTE ,61000SUB#15 IS BONUS
	PLEASE NOTE ,61000SUB#16 IS CLOCK1
	PLEASE NOTE ,61000SUB#17 IS CLOCK2
	PLEASE NOTE ,61000SUB#18 IS OLDLC2
	PLEASE NOTE ,61000SUB#19 IS DARK
	PLEASE NOTE ,61000SUB#20 IS WZDARK IS WHETHER THE LOC HE'S LEAVING WAS DARK
	PLEASE NOTE ,61000SUB#21 IS VERB
	PLEASE NOTE ,61000SUB#22 IS OBJ
	PLEASE NOTE ,61000SUB#23 IS OLD_OBJ
	PLEASE NOTE
	PLEASE NOTE ,61001 IS LOCATION STATE
	PLEASE NOTE ,61001SUB.LOC#1 IS ABB(LOC) IS HOW MANY TIMES LOCATION WAS DESCRIBED
	PLEASE NOTE
	PLEASE NOTE ,61002 IS DWARVES STATE
	PLEASE NOTE ,61002SUB.DWARF#1 IS LOCATION
	PLEASE NOTE ,61002SUB.DWARF#2 IS SEEN FLAG

(61000)	DO ,61000 <- #23
	DO ,61000SUB#1 <- #1
	DO ,61000SUB#2 <- #1
	DO ,61000SUB#5 <- #330
	DO ,61000SUB#9 <- #5

	DO ,61001 <- #140 BY #1

	DO ,61002 <- #6 BY #2
	DO ,61002SUB#1#1 <- #19
	DO ,61002SUB#2#1 <- #27
	DO ,61002SUB#3#1 <- #33
	DO ,61002SUB#4#1 <- #44
	DO ,61002SUB#5#1 <- #64
	DO ,61002SUB#6#1 <- #114
	DO RESUME #1
