with TEXT_IO;
with WORD_PARAMETERS; use WORD_PARAMETERS;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
procedure LIST_LIST(PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER) is

  use INFLECTION_RECORD_IO;
  use DICT_IO;

  PR, OPR : PARSE_RECORD;
  J : INTEGER := 0;
  

  procedure ORDER_PARSE_ARRAY(SX : in out PARSE_ARRAY) is
     use INFLECTION_RECORD_IO;
     use DICT_IO;

     HITS : INTEGER := 0;
     SL : PARSE_ARRAY(SX'FIRST..SX'LAST) := SX;
     SL_LAST : INTEGER := SL'LAST;
     SM : PARSE_RECORD;

     NOT_ONLY_ARCHAIC  : BOOLEAN := FALSE;
     NOT_ONLY_MEDIEVAL : BOOLEAN := FALSE;
     NOT_ONLY_UNCOMMON : BOOLEAN := FALSE;
     J : INTEGER := SL'FIRST;

   begin
     PA_LAST := PA'LAST;
     
    if SX'LENGTH = 0              then
        return;
     end if;



--  Bubble sort since this list should usually be very small (1-5)
    HIT_LOOP:
    loop
      HITS := 0;


     --------------------------------------------------


      SWITCH:
      declare

        function "<" (LEFT, RIGHT : QUALITY_RECORD) return BOOLEAN is
        begin
          if LEFT.POFS = RIGHT.POFS  and then
             LEFT.POFS = PRON        and then
             LEFT.PRON.DECL.WHICH = 1    then
            return (LEFT.PRON.DECL.VAR < RIGHT.PRON.DECL.VAR);
          else
            return INFLECTIONS_PACKAGE."<"(LEFT, RIGHT);
          end if;
        end "<";

       function EQU (LEFT, RIGHT : QUALITY_RECORD) return BOOLEAN is
        begin

          if LEFT.POFS = RIGHT.POFS  and then
             LEFT.POFS = PRON        and then
             LEFT.PRON.DECL.WHICH = 1    then

            return (LEFT.PRON.DECL.VAR = RIGHT.PRON.DECL.VAR);
          else

            return INFLECTIONS_PACKAGE."="(LEFT, RIGHT);
          end if;

        end EQU;

          function MEANING (PR : PARSE_RECORD) return MEANING_TYPE is
          DE : DICTIONARY_ENTRY;
        begin
          DICT_IO.SET_INDEX(DICT_FILE(PR.D_K), PR.MNPC);
          DICT_IO.READ(DICT_FILE(PR.D_K), DE);
          return DE.MEAN;
        end MEANING;

      begin
      --  Need to remove duplicates in ARRAY_STEMS
      --  This sort is very sloppy
      --  One problem is that it can mix up some of the order of PREFIX, XXX, LOC
      --  I ought to do this for every set of results from different approaches
      --  not just in one fell swoop at the end !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  
          INNER_LOOP:
          for I in SL'FIRST..SL_LAST-1  loop
            --  Maybe <   =  on PR.STEM  -  will have to make up "<"   --  Actually STEM and PART  --  and check that later in print

            if SL(I+1).D_K  > SL(I).D_K   or else  --  Let DICT.LOC list first

               (SL(I+1).D_K  = SL(I).D_K    and then
                SL(I+1).IR.QUAL < SL(I).IR.QUAL)  or else

               (SL(I+1).D_K  = SL(I).D_K    and then
                EQU(SL(I+1).IR.QUAL, SL(I).IR.QUAL)  and then
                MEANING(SL(I+1)) < MEANING(SL(I)))  or else   --  | is > letter

               (SL(I+1).D_K  = SL(I).D_K  and then
                EQU(SL(I+1).IR.QUAL, SL(I).IR.QUAL)  and then
                MEANING(SL(I+1)) = MEANING(SL(I))   and then
                SL(I+1).IR.ENDING.SIZE < SL(I).IR.ENDING.SIZE)    or else

               (SL(I+1).D_K  = SL(I).D_K  and then
                EQU(SL(I+1).IR.QUAL, SL(I).IR.QUAL)  and then
                MEANING(SL(I+1)) = MEANING(SL(I))   and then
                SL(I+1).IR.ENDING.SIZE = SL(I).IR.ENDING.SIZE  and then
                INFLECTIONS_PACKAGE."<"(SL(I+1).IR.QUAL, SL(I).IR.QUAL))
                                                                 then


               SM := SL(I);
               SL(I) := SL(I+1);
               SL(I+1) := SM;
               HITS := HITS + 1;

            end if;

         end loop INNER_LOOP;

--for I in 1..SX'LAST loop
--  TEXT_IO.PUT_LINE(INTEGER'IMAGE(I) & MEANING(SX(I)));
--end loop;
        end SWITCH;
     --------------------------------------------------


        exit when HITS = 0;
     end loop HIT_LOOP;


     --  Fix up the Archaic/Medieval
     if WORDS_MODE(TRIM_OUTPUT)  then
     --  Remove those inflections if MDEV and there is other valid
     for I in SL'FIRST..SL_LAST  loop
       if SL(I).IR.AGE > A  then
         NOT_ONLY_ARCHAIC := TRUE;
       elsif SL(I).IR.AGE < E  then
         NOT_ONLY_MEDIEVAL := TRUE;
       end if;
       if SL(I).IR.FREQ < C then         --  C for inflections is uncommon  !!!!
         NOT_ONLY_UNCOMMON := TRUE;
       end if;
     end loop;

     J := SL'FIRST;
     while J <= SL_LAST  loop
       if (NOT_ONLY_ARCHAIC and WORDS_MDEV(OMIT_ARCHAIC)) and then
           SL(J).IR.AGE = A  then
         SL(J) := NULL_PARSE_RECORD;  -- Signaling no record
       end if;
       if (NOT_ONLY_MEDIEVAL and WORDS_MDEV(OMIT_MEDIEVAL)) and then
           SL(J).IR.AGE >= E  then
          SL(J) := NULL_PARSE_RECORD;  -- Signaling no record
       end if;
       if (NOT_ONLY_UNCOMMON and WORDS_MDEV(OMIT_UNCOMMON)) and then
           SL(J).IR.FREQ >= C  then
         SL(J) := NULL_PARSE_RECORD;  -- Signaling no record
       end if;
       J := J + 1;
     end loop;

     end if;   --  On TRIM

     SX := SL;


  end ORDER_PARSE_ARRAY;


  function ALLOWED_STEM(PR : PARSE_RECORD) return BOOLEAN is
     ALLOWED : BOOLEAN := TRUE;   --  modify as necessary and return it
     DE : DICTIONARY_ENTRY;
  begin
    if PR. D_K not in GENERAL..LOCAL  then return TRUE; end if;
    
    DICT_IO.SET_INDEX(DICT_FILE(PR.D_K), PR.MNPC);
    DICT_IO.READ(DICT_FILE(PR.D_K), DE);

    --  NOUN CHECKS

     if PR.IR.QUAL.POFS = N  then            --  if NOUN

--     --  Check for Vocative being person/name and Locative a place/area
--        if (PR.IR.QUAL.N.CS = VOC) and then 
--           ((PR.IR.QUAL.N.KIND /= N) and (PR.IR.QUAL.N.KIND /= P)) then
----PUT("N VOC not a P or N           "); PUT(PR.IR); NEW_LINE;
--           ALLOWED := FALSE;
--        elsif (PR.IR.QUAL.N.CS = LOC) and then 
--           ((PR.IR.QUAL.N.KIND /= NOUN_KIND_TYPE'(L)) and 
--               (PR.IR.QUAL.N.KIND /= NOUN_KIND_TYPE'(W))) then
----PUT("N LOC not a L or W           "); PUT(PR.IR); NEW_LINE;
--           ALLOWED := FALSE;
--        end if;

        if  WORDS_MDEV(FOR_WORD_LIST_CHECK)  then
           if (NOM <= PR.IR.QUAL.N.CS) and then
              (S <= PR.IR.QUAL.N.NUMBER) then
              ALLOWED := TRUE;
           elsif (NOM <= PR.IR.QUAL.N.CS) and then
              (PR.IR.QUAL.N.NUMBER = P) then
              SEARCH_FOR_PL:
              declare
                 DE : DICTIONARY_ENTRY;
                 MEAN : MEANING_TYPE := NULL_MEANING_TYPE;
              begin
                 ALLOWED := FALSE;
                 DICT_IO.READ(DICT_FILE(PR.D_K), DE, PR.MNPC);
                 MEAN := DE.MEAN;
                 for J in MEANING_TYPE'FIRST..MEANING_TYPE'LAST-2  loop
                    if MEAN(J..J+2) = "pl."  then
                       ALLOWED := TRUE;
                       exit;
                    end if;
                 end loop;
              end SEARCH_FOR_PL;
           --====================================
           else
              ALLOWED := FALSE;
           end if;
        end if;

     end if;      --  if NOUN                           

     if PR.IR.QUAL.POFS = ADJ  then           --  if ADJ
        if  WORDS_MDEV(FOR_WORD_LIST_CHECK)  then
           if (NOM <= PR.IR.QUAL.ADJ.CS) and then
              (S <= PR.IR.QUAL.ADJ.NUMBER) and then
              (M <= PR.IR.QUAL.ADJ.GENDER)  then
              ALLOWED := TRUE;
           else
              ALLOWED := FALSE;
           end if;
        end if;

     end if;      --  if ADJ                            

     --  VERB CHECKS

     if PR.IR.QUAL.POFS = V  then             --  if VERB
     --  Check for Verb Imperative being in permitted person
        if (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD = IMP) then
           if (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE = PRES) and
              (PR.IR.QUAL.V.PERSON = 2)  then
              null;
           elsif (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE = FUT) and
              (PR.IR.QUAL.V.PERSON = 2 or PR.IR.QUAL.V.PERSON = 3)  then
              null;
           else
           --PUT("IMP not in permitted person  "); PUT(PR.IR); NEW_LINE;
              ALLOWED := FALSE;
           end if;
        end if;

     --  Check for V IMPERS and demand that only 3rd person    --  ???????
        if (DE.KIND.V_KIND = IMPERS) then
           if (PR.IR.QUAL.V.PERSON = 3)  then
              null;
           else
           --PUT("IMPERS not in 3rd person     "); PUT(PR.IR); NEW_LINE;
              ALLOWED := FALSE;
           end if;
        end if;

     --  Check for V DEP    and demand PASSIVE   
        if (DE.KIND.V_KIND = DEP) then
           if (PR.IR.QUAL.V.TENSE_VOICE_MOOD.VOICE = PASSIVE)  and
              (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD = INF)  and
              (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE = FUT)  then
           --PUT("DEP    FUT INF not in ACTIVE "); PUT(PR.IR); NEW_LINE;
              ALLOWED := FALSE;
           elsif (PR.IR.QUAL.V.TENSE_VOICE_MOOD.VOICE = ACTIVE)  and
              (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD in IND..INF)  then
           --PUT("DEP    not in PASSIVE        "); PUT(PR.IR); NEW_LINE;
              ALLOWED := FALSE;
           else
              null;
           end if;
        end if;

     --  Check for V SEMIDEP    and demand PASSIVE ex Perf  
        if (DE.KIND.V_KIND = SEMIDEP) then
           if (PR.IR.QUAL.V.TENSE_VOICE_MOOD.VOICE = PASSIVE)  and
              (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE in PRES..FUT)  and
              (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD in IND..IMP)  then
           --PUT("SEMIDEP    Pres not in ACTIVE "); PUT(PR.IR); NEW_LINE;
              ALLOWED := FALSE;
           elsif (PR.IR.QUAL.V.TENSE_VOICE_MOOD.VOICE = ACTIVE)  and
              (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE in PERF..FUTP )  and
              (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD in IND..IMP)  then
           --PUT("SEMIDEP    Perf not in PASSIVE "); PUT(PR.IR); NEW_LINE;
              ALLOWED := FALSE;
           else
              null;
           end if;
        end if;


        if (DE.KIND.V_KIND = DEP) then
           if (PR.IR.QUAL.V.TENSE_VOICE_MOOD.VOICE = PASSIVE)  and
              (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD = INF)  and
              (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE = FUT)  then
           --PUT("DEP    FUT INF not in ACTIVE "); PUT(PR.IR); NEW_LINE;
              ALLOWED := FALSE;
           elsif (PR.IR.QUAL.V.TENSE_VOICE_MOOD.VOICE = ACTIVE)  and
              (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD in IND..INF)  then
           --PUT("DEP    not in PASSIVE        "); PUT(PR.IR); NEW_LINE;
              ALLOWED := FALSE;
           else
              null;
           end if;
        end if;

        if  WORDS_MDEV(FOR_WORD_LIST_CHECK)  then
           if (PR.IR.QUAL.V.PERSON = 1) and then
              (PR.IR.QUAL.V.NUMBER = S)  then
              if ((DE.KIND.V_KIND in X..INTRANS)  and
                     (PR.IR.QUAL.V.TENSE_VOICE_MOOD = (PRES, ACTIVE, IND))) or else
                 ((DE.KIND.V_KIND = DEP)  and
                     (PR.IR.QUAL.V.TENSE_VOICE_MOOD = (PRES, PASSIVE, IND))) or else
                 ((DE.KIND.V_KIND = SEMIDEP)  and
                     (PR.IR.QUAL.V.TENSE_VOICE_MOOD = (PRES, ACTIVE, IND))) then
                 ALLOWED := TRUE;
              elsif ((DE.KIND.V_KIND = PERFDEF)  and
                        (PR.IR.QUAL.V.TENSE_VOICE_MOOD = (PERF, ACTIVE, IND))) then
                 ALLOWED := TRUE;
              else
                 ALLOWED := FALSE;
              end if;
           elsif (DE.KIND.V_KIND = IMPERS) then
              if (PR.IR.QUAL.V.PERSON = 3)  and then
                 (PR.IR.QUAL.V.NUMBER = S)  and then
                 (PR.IR.QUAL.V.TENSE_VOICE_MOOD = (PRES, ACTIVE, IND))   then
                 ALLOWED := TRUE;
              else
                 ALLOWED := FALSE;
              end if;
           else
              ALLOWED := FALSE;
           end if;
        end if;


     end if;        --  if VERB

     if  WORDS_MDEV(FOR_WORD_LIST_CHECK)   then       --  Non parts
        if (PR.IR.QUAL.POFS in VPAR..SUPINE)    then
           ALLOWED := FALSE;
        end if;
     end if;                                           --  Non parts

     return ALLOWED;

  end ALLOWED_STEM;

  begin
   
  if PA'LENGTH = 0              then
    return;
  end if;
  
  PA_LAST := PA'LAST;
   
--TEXT_IO.PUT_LINE("PA on entering LIST_LIST     PA'LAST = " & INTEGER'IMAGE(PA'LAST));
--for I in 1..PA'LAST  loop
--PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
--end loop;

RESET_PRONOUN_KIND:
declare
  DE : DICTIONARY_ENTRY;
begin
for I in 1..PA'LAST  loop
  if PA(I).D_K = GENERAL  then
    DICT_IO.SET_INDEX(DICT_FILE(PA(I).D_K), PA(I).MNPC); 
    DICT_IO.READ(DICT_FILE(PA(I).D_K), DE);
    if PA(I).IR.QUAL.POFS = PRON  and then
      PA(I).IR.QUAL.PRON.DECL.WHICH =1  then
      PA(I).IR.QUAL.PRON.DECL.VAR := PRONOUN_KIND_TYPE'POS(DE.KIND.PRON_KIND);
    end if;
  end if;
end loop;
end RESET_PRONOUN_KIND;

---------------------------------------------------




     SWEEPING:
     --  To remove disallowed stems/inflections and resulting dangling fixes
     declare
        FIX_ON : BOOLEAN := FALSE;
        PW_ON  : BOOLEAN := FALSE;
        P_FIRST : INTEGER := 1;
        P_LAST  : INTEGER := 0;
        subtype XONS is PART_OF_SPEECH_TYPE range TACKON..SUFFIX;

     
     begin

--TEXT_IO.NEW_LINE;
--TEXT_IO.PUT_LINE("SWEEPING    ======================================");
--TEXT_IO.NEW_LINE;
--
        J := PA_LAST;

        while J >= 1  loop        --  Sweep backwards over PA


--TEXT_IO.PUT(INTEGER'IMAGE(J)); TEXT_IO.PUT("  =>   ");
--TEXT_IO.PUT_LINE(PA(J).STEM);
--INFLECTION_RECORD_IO.PUT(TEXT_IO.STANDARD_OUTPUT, PA(J).IR);
--
           if (not ALLOWED_STEM(PA(J))   or
               (PA(J) = NULL_PARSE_RECORD))  then
--TEXT_IO.PUT(INTEGER'IMAGE(J)); TEXT_IO.PUT_LINE("  not ALLOWED");
              PA(J..PA_LAST-1) := PA(J+1..PA_LAST);
              PA_LAST := PA_LAST - 1;
              P_LAST := P_LAST - 1;
--TEXT_IO.PUT_LINE(" -------------- " & INTEGER'IMAGE(PA_LAST));

           elsif ((PA(J).D_K in ADDONS..YYY) or (PA(J).IR.QUAL.POFS in XONS))   and then
                 (PW_ON)     then               --  first FIX/TRICK after regular
--TEXT_IO.PUT(INTEGER'IMAGE(J)); TEXT_IO.PUT_LINE("  XONS/X  & PW_ON");
              FIX_ON := TRUE;
--TEXT_IO.PUT_LINE("FIX_ON turned ON");
                       PW_ON  := FALSE;
--TEXT_IO.PUT_LINE("PW_ON turned OFF");
              P_FIRST := J + 1;
--TEXT_IO.PUT_LINE("                  " &  INTEGER'IMAGE(PA_LAST));


             ----Order internal to this set of inflections
--TEXT_IO.PUT_LINE("ORDER   FIX   P_FIRST  P_LAST   " & INTEGER'IMAGE(P_FIRST) & "  " & INTEGER'IMAGE(P_LAST));
              ORDER_PARSE_ARRAY(PA(P_FIRST..P_LAST));

              P_FIRST := 1;
              P_LAST  := 0;


           elsif ((PA(J).D_K in ADDONS..YYY) or (PA(J).IR.QUAL.POFS in XONS))  and then
                 (FIX_ON)     then               --  another FIX
--TEXT_IO.PUT(INTEGER'IMAGE(J)); TEXT_IO.PUT("  XONS else");
--TEXT_IO.PUT_LINE("                 " & INTEGER'IMAGE(PA_LAST));
              null;

           elsif ((PA(J).D_K in ADDONS..YYY)  or
                     (PA(J).IR.QUAL.POFS = X))  and then  --  Kills TRICKS stuff
              (not PW_ON)     then
--TEXT_IO.PUT(INTEGER'IMAGE(J)); TEXT_IO.PUT_LINE("  XONS or X  and not PW_ON");
              PA(J..PA_LAST-1) := PA(J+1..PA_LAST);
              PA_LAST := PA_LAST - 1;
              P_LAST := P_LAST - 1;
--TEXT_IO.PUT_LINE(" -------------- " & INTEGER'IMAGE(PA_LAST));

           else
--TEXT_IO.PUT("  =>   J = " & INTEGER'IMAGE(J)); TEXT_IO.PUT_LINE("   else");
              PW_ON := TRUE;
--TEXT_IO.PUT_LINE("PW_ON turned ON");
              FIX_ON := FALSE;
--TEXT_IO.PUT_LINE("FIX_ON turned OFF");
              if P_LAST <= 0  then
                 P_LAST := J;
              end if;
              if J = 1  then
--TEXT_IO.PUT_LINE("J = 1 therefore ORDER 1..P_LAST    P_LAST = " & INTEGER'IMAGE(P_LAST));
--TEXT_IO.PUT_LINE("ORDER   else  P_FIRST  P_LAST   " & INTEGER'IMAGE(P_FIRST) & "  " & INTEGER'IMAGE(P_LAST));
                 ORDER_PARSE_ARRAY(PA(1..P_LAST));
              end if;
--TEXT_IO.PUT_LINE("                " & INTEGER'IMAGE(PA_LAST));

           end if;                                      --  check PART
           J := J - 1;
        end loop;                          --  loop sweep over PA

     end SWEEPING;

--TEXT_IO.PUT_LINE("PA after SWEEPING  in LIST_STEMS - before COMPRESS_LOOP");
--for I in 1..PA_LAST  loop
--PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
--end loop;

     OPR := PA(1);
  --  Last chance to weed out duplicates
     J := 2;
  COMPRESS_LOOP:
     loop
        exit when J > PA_LAST;
        PR := PA(J);
        if PR /= OPR  then
           SUPRESS_KEY_CHECK:
           declare
              function "<=" (A, B : PARSE_RECORD) return BOOLEAN is
              begin                             --  !!!!!!!!!!!!!!!!!!!!!!!!!!
                 if A.IR.QUAL = B.IR.QUAL  and
                    A.MNPC    = B.MNPC     then
                    return TRUE;
                 else
                    return FALSE;
                 end if;
              end "<=";
              function "<" (A, B : PARSE_RECORD) return BOOLEAN is
              begin                             --  !!!!!!!!!!!!!!!!!!!!!!!!!!
                 if A.IR.QUAL = B.IR.QUAL  and
                 A.MNPC   /= B.MNPC     then
                    return TRUE;
                 else
                    return FALSE;
                 end if;
              end "<";
           begin
              if ((PR.D_K /= XXX) and (PR.D_K /= YYY) and  (PR.D_K /= PPP)) then
                if PR <= OPR  then       --  Get rid of duplicates, if ORDER is OK
--TEXT_IO.PUT_LINE("   1   ");
                   PA(J.. PA_LAST-1) := PA(J+1..PA_LAST);  --  Shift PA down 1
                   PA_LAST := PA_LAST - 1;        --  because found key duplicate
--                elsif PR < OPR  then       --  Compress meanings   --  no longer applicable
----TEXT_IO.PUT_LINE("   2   ");
--                  PA(J.. PA_LAST-1) := PA(J+1..PA_LAST);  --  Shift PA down 1
--                   PA_LAST := PA_LAST - 1;        --  because found  duplicate
                end if;
              else
                 J := J + 1;
--TEXT_IO.PUT_LINE("   3   ");
              end if;
           end SUPRESS_KEY_CHECK;
         else
           J := J + 1;
                  
--TEXT_IO.PUT_LINE("   4   ");
--           PA(J.. PA_LAST-1) := PA(J+1..PA_LAST);  --  Shift PA down 1
--           PA_LAST := PA_LAST - 1;            --  because found exact duplicate
        end if;
        OPR := PR;
     end loop COMPRESS_LOOP;



--  Distroy the artificial VAR
for I in 1..PA'LAST  loop
  if PA(I).IR.QUAL.POFS = PRON  and then
     PA(I).IR.QUAL.PRON.DECL.WHICH =1  then
    PA(I).IR.QUAL.PRON.DECL.VAR := 0;
  end if;
end loop;



end LIST_LIST;
