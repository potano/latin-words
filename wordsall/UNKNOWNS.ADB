

      procedure UNKNOWN_SEARCH(UNKNOWN : STEM_TYPE) is
     
         use STEM_IO;
         D_K : constant DICTIONARY_KIND := GENERAL;
         
         J, J1, J2, JJ : STEM_IO.COUNT := 0;

         INDEX_ON : constant STRING := UNKNOWN;
         INDEX_FIRST, INDEX_LAST : STEM_IO.COUNT := 0;
         DS : DICTIONARY_STEM;
         FIRST_TRY, SECOND_TRY : BOOLEAN := TRUE;


         function FIRST_TWO(W : STRING) return STRING is
         --  'v' could be represented by 'u', like the new Oxford Latin Dictionary
         --  Fixes the first two letters of a word/stem which can be done right
            S : constant STRING := LOWER_CASE(W);
            SS : STRING(W'RANGE) := W;
            
            function UI(C : CHARACTER) return CHARACTER  is
            begin
               if (C = 'v')   then
                  return 'u';
               elsif (C = 'V')  then
                  return 'U';
               elsif (C = 'j')  then
                  return 'i';
               elsif (C = 'J')  then
                  return 'I';
               else
                  return C;
               end if;
            end UI;

         begin

            if S'LENGTH = 1  then
               SS(S'FIRST) := UI(W(S'FIRST));
            else
               SS(S'FIRST)   := UI(W(S'FIRST));
               SS(S'FIRST+1) := UI(W(S'FIRST+1));
            end if;

            return SS;
         end FIRST_TWO;


         procedure LOAD_PDL is
         begin
            case RESTRICTION is
               when REGULAR    =>
                  if not (DS.PART.POFS = PACK  or
                             (DS.PART.POFS = PRON  and then
                                 (DS.PART.PRON.DECL.WHICH = 1)))  then
                     PDL_INDEX := PDL_INDEX + 1;
                     PDL(PDL_INDEX) := PRUNED_DICTIONARY_ITEM'(DS, D_K);
                  end if;

               when PACK_ONLY  =>
                  if DS.PART.POFS = PACK  then
                     PDL_INDEX := PDL_INDEX + 1;
                     PDL(PDL_INDEX) := PRUNED_DICTIONARY_ITEM'(DS, D_K);
                  end if;

               when QU_PRON_ONLY  =>
                  if DS.PART.POFS = PRON  and then
                     (DS.PART.PRON.DECL.WHICH = 1)  then
                     PDL_INDEX := PDL_INDEX + 1;
                     PDL(PDL_INDEX) := PRUNED_DICTIONARY_ITEM'(DS, D_K);
                  end if;

               when others =>
                  PDL_INDEX := PDL_INDEX + 1;
                  PDL(PDL_INDEX) := PRUNED_DICTIONARY_ITEM'(DS, D_K);
            end case;

         end LOAD_PDL;


      begin
           
         INDEX_FIRST := FIRST_INDEX(FIRST_TWO(INDEX_ON), D_K);
         INDEX_LAST  := LAST_INDEX(FIRST_TWO(INDEX_ON), D_K);
         
         if INDEX_FIRST > 0  and then INDEX_FIRST <= INDEX_LAST then


            J1 := STEM_IO.COUNT(INDEX_FIRST);    --######################
            J2 := STEM_IO.COUNT(INDEX_LAST);

           

                     FIRST_TRY := TRUE;

                     SECOND_TRY := TRUE;

                     J := (J1 + J2) / 2;

                  BINARY_SEARCH:
                     loop

                        if (J1 = J2-1) or (J1 = J2) then
                           if FIRST_TRY  then
                           --TEXT_IO.PUT_LINE("FIRST_TRY");
                              J := J1;
                              FIRST_TRY := FALSE;
                           elsif SECOND_TRY  then
                           --TEXT_IO.PUT_LINE("SECOND_TRY");
                              J := J2;
                              SECOND_TRY := FALSE;
                           else
                           --TEXT_IO.PUT_LINE("THIRD_TRY   exit BINARY_SEARCH");
                              JJ := J;
                              exit BINARY_SEARCH;
                           end if;
                        end if;

                        SET_INDEX(STEM_FILE(D_K), STEM_IO.COUNT(J));
                        READ(STEM_FILE(D_K), DS);
                     
                        if  LTU(LOWER_CASE(DS.STEM), UNKNOWN)  then
                           J1 := J;
                           J := (J1 + J2) / 2;
                        elsif  GTU(LOWER_CASE(DS.STEM), UNKNOWN)  then
                           J2 := J;
                           J := (J1 + J2) / 2;
                        else
                           for I in reverse J1..J  loop
                              SET_INDEX(STEM_FILE(D_K), STEM_IO.COUNT(I));
                              READ(STEM_FILE(D_K), DS);
                           
                              if EQU(LOWER_CASE(DS.STEM), UNKNOWN)  then
                                 JJ := I;
                                 LOAD_PDL;

                              else
                                 exit;
                              end if;
                           end loop;

                           for I in J+1..J2  loop
                              SET_INDEX(STEM_FILE(D_K), STEM_IO.COUNT(I));
                              READ(STEM_FILE(D_K), DS);
                           
                              if EQU(LOWER_CASE(DS.STEM), UNKNOWN)  then
                                JJ := I;

                                 LOAD_PDL;

                              else
                                 exit BINARY_SEARCH;
                              end if;
                           end loop;
                           exit BINARY_SEARCH;

                        end if;
                     end loop BINARY_SEARCH;
                     J1 := JJ;
                     J2 := STEM_IO.COUNT(INDEX_LAST);

                  end if;               --  On LOCAL check
            
         end if;
      
      end UNKNOWN_SEARCH;


