with STRINGS_PACKAGE; use STRINGS_PACKAGE;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
  function DICTIONARY_FORM(DE : DICTIONARY_ENTRY;
                  LINE_NUMBER : INTEGER := 0) return STRING is

      NULL_OX : constant STRING(1..24) := (others => ' ');
      OX : array (1..4) of STRING (1..24) := (others => NULL_OX);
      FORM : STRING(1..100) := (others => ' ');

      NOT_FOUND : exception;

      function ADD(STEM, INFL : STRING) return STRING is
      begin
        return HEAD(TRIM(STEM) & TRIM(INFL), 24);
      end ADD;

      procedure ADD_UP(FACTOR : STRING) is
      begin
        FORM := HEAD(TRIM(FORM) & TRIM(FACTOR), 100);
      end ADD_UP;

    begin


        if DE.PART.PART = N    then
          if DE.PART.N.DECL.WHICH = 1  then
            if DE.PART.N.DECL.VAR = 1  then
              OX(1) := ADD(DE.STEMS(1), "a");
              OX(2) := ADD(DE.STEMS(2), "ae");
            elsif DE.PART.N.DECL.VAR = 6  then
              OX(1) := ADD(DE.STEMS(1), "e");
              OX(2) := ADD(DE.STEMS(2), "es");
            elsif DE.PART.N.DECL.VAR = 7  then
              OX(1) := ADD(DE.STEMS(1), "es");
              OX(2) := ADD(DE.STEMS(2), "ae");
            elsif DE.PART.N.DECL.VAR = 8  then
              OX(1) := ADD(DE.STEMS(1), "as");
              OX(2) := ADD(DE.STEMS(2), "ae");
            end if;

          elsif DE.PART.N.DECL.WHICH = 2  then
            if DE.PART.N.DECL.VAR = 1  then
              OX(1) := ADD(DE.STEMS(1), "us");
              OX(2) := ADD(DE.STEMS(2), "i");
            elsif DE.PART.N.DECL.VAR = 2  then
              OX(1) := ADD(DE.STEMS(1), "um");
              OX(2) := ADD(DE.STEMS(2), "i");
            elsif DE.PART.N.DECL.VAR = 3  then
              OX(1) := ADD(DE.STEMS(1), "");
              OX(2) := ADD(DE.STEMS(2), "i");
            elsif DE.PART.N.DECL.VAR = 4  then
              if DE.PART.N.GENDER = N  then
                OX(1) := ADD(DE.STEMS(1), "um");
              else
                OX(1) := ADD(DE.STEMS(1), "us");
              end if;
              OX(2) := ADD(DE.STEMS(2), "(i)");
            elsif DE.PART.N.DECL.VAR = 5  then
              OX(1) := ADD(DE.STEMS(1), "us");
              OX(2) := ADD(DE.STEMS(2), "");
            elsif DE.PART.N.DECL.VAR = 6  then
              OX(1) := ADD(DE.STEMS(1), "os");
              OX(2) := ADD(DE.STEMS(2), "i");
            elsif DE.PART.N.DECL.VAR = 7  then
              OX(1) := ADD(DE.STEMS(1), "os");
              OX(2) := ADD(DE.STEMS(2), "i");
            elsif DE.PART.N.DECL.VAR = 8  then
              OX(1) := ADD(DE.STEMS(1), "on");
              OX(2) := ADD(DE.STEMS(2), "i");
            elsif DE.PART.N.DECL.VAR = 9  then
              OX(1) := ADD(DE.STEMS(1), "us");
              OX(2) := ADD(DE.STEMS(2), "i");
            end if;

          elsif DE.PART.N.DECL.WHICH = 3  then
            OX(1) := ADD(DE.STEMS(1), "");
            if (DE.PART.N.DECL.VAR = 7)  or
               (DE.PART.N.DECL.VAR = 9)  then
              OX(2) := ADD(DE.STEMS(2), "os/is");
            else
              OX(2) := ADD(DE.STEMS(2), "is");
            end if;

          elsif DE.PART.N.DECL.WHICH = 4  then
            if DE.PART.N.DECL.VAR = 1  then
              OX(1) := ADD(DE.STEMS(1), "us");
              OX(2) := ADD(DE.STEMS(2), "us");
            elsif DE.PART.N.DECL.VAR = 2  then
              OX(1) := ADD(DE.STEMS(1), "u");
              OX(2) := ADD(DE.STEMS(2), "us");
            elsif DE.PART.N.DECL.VAR = 3  then
              OX(1) := ADD(DE.STEMS(1), "us");
              OX(2) := ADD(DE.STEMS(2), "u");
            end if;

          elsif DE.PART.N.DECL.WHICH = 5  then
              OX(1) := ADD(DE.STEMS(1), "es");
              OX(2) := ADD(DE.STEMS(2), "ei");

          elsif DE.PART.N.DECL = (9, 8)  then
            OX(1) := ADD(DE.STEMS(1), "");
            OX(2) := ADD(NULL_OX, "abb.");

          elsif DE.PART.N.DECL = (9, 9)  then
            OX(1) := ADD(DE.STEMS(1), "");
            OX(2) := ADD(NULL_OX, "undeclined");

          else
            raise NOT_FOUND;
          end if;

        elsif DE.PART.PART = PRON    then
          if DE.PART.PRON.DECL.WHICH = 3  then
            OX(1) := ADD(DE.STEMS(1), "ic");
            OX(2) := ADD(DE.STEMS(1), "aec");
            if DE.PART.PRON.DECL.VAR = 1  then
              OX(3) := ADD(DE.STEMS(1), "oc");
            elsif DE.PART.PRON.DECL.VAR = 2  then
              OX(3) := ADD(DE.STEMS(1), "uc");
            end if;

          elsif DE.PART.PRON.DECL.WHICH = 4  then
            if DE.PART.PRON.DECL.VAR = 1  then
              OX(1) := ADD(DE.STEMS(1), "s");
              OX(2) := ADD(DE.STEMS(2), "a");
              OX(3) := ADD(DE.STEMS(1), "d");
            elsif DE.PART.PRON.DECL.VAR = 2  then
              OX(1) := ADD(DE.STEMS(1), "dem");
              OX(2) := ADD(DE.STEMS(2), "adem");
              OX(3) := ADD(DE.STEMS(1), "dem");
            end if;


          elsif DE.PART.PRON.DECL.WHICH = 6  then
            OX(1) := ADD(DE.STEMS(1), "e");
            OX(2) := ADD(DE.STEMS(1), "a");
            if DE.PART.PRON.DECL.VAR = 1  then
              OX(3) := ADD(DE.STEMS(1), "ud");
            elsif DE.PART.PRON.DECL.VAR = 2  then
              OX(3) := ADD(DE.STEMS(1), "um");
            end if;

          elsif DE.PART.PRON.DECL = (9, 9)  then
            OX(1) := ADD(DE.STEMS(1), "");
            OX(2) := ADD(NULL_OX, "undeclined");

          else
            raise NOT_FOUND;
          end if;

        elsif DE.PART.PART = ADJ  then
          if DE.PART.ADJ.CO = COMP  then
            OX(1) := ADD(DE.STEMS(1), "or");
            OX(2) := ADD(DE.STEMS(1), "or");
            OX(3) := ADD(DE.STEMS(1), "us");
          elsif DE.PART.ADJ.CO = SUPER  then
            OX(1) := ADD(DE.STEMS(1), "mus");
            OX(2) := ADD(DE.STEMS(1), "ma");
            OX(3) := ADD(DE.STEMS(1), "mum");

          elsif DE.PART.ADJ.CO = POS  then
            if DE.PART.ADJ.DECL.WHICH = 1  then
              if DE.PART.ADJ.DECL.VAR = 1  then
                OX(1) := ADD(DE.STEMS(1), "us");
                OX(2) := ADD(DE.STEMS(2), "a");
                OX(3) := ADD(DE.STEMS(2), "um");
              elsif DE.PART.ADJ.DECL.VAR = 2  then
                OX(1) := ADD(DE.STEMS(1), "");
                OX(2) := ADD(DE.STEMS(2), "a");
                OX(3) := ADD(DE.STEMS(2), "um");
              elsif DE.PART.ADJ.DECL.VAR = 3  then
                OX(1) := ADD(DE.STEMS(1), "us");
                OX(2) := ADD(DE.STEMS(2), "a");
                OX(3) := ADD(DE.STEMS(2), "um (gen -ius)");
              elsif DE.PART.ADJ.DECL.VAR = 4  then
                OX(1) := ADD(DE.STEMS(1), "");
                OX(2) := ADD(DE.STEMS(2), "a");
                OX(3) := ADD(DE.STEMS(2), "um");
              elsif DE.PART.ADJ.DECL.VAR = 5  then
                OX(1) := ADD(DE.STEMS(1), "us");
                OX(2) := ADD(DE.STEMS(2), "a");
                OX(3) := ADD(DE.STEMS(2), "ud");
              elsif DE.PART.ADJ.DECL.VAR = 6  then
                OX(1) := ADD(NULL_OX, "-");
                OX(2) := ADD(DE.STEMS(2), "e");
                OX(3) := ADD(NULL_OX, "-");
              elsif DE.PART.ADJ.DECL.VAR = 7  then
                OX(1) := ADD(DE.STEMS(1), "es");
                OX(2) := ADD(DE.STEMS(1), "es");
                OX(3) := ADD(DE.STEMS(1), "es");
              end if;

            elsif DE.PART.ADJ.DECL.WHICH = 2  then
              if DE.PART.ADJ.DECL.VAR = 6  then
                OX(1) := ADD(DE.STEMS(1), "os");
                OX(2) := ADD(DE.STEMS(1), "os");
                OX(3) := ADD(NULL_OX, "-");
              elsif DE.PART.ADJ.DECL.VAR = 7  then
                OX(1) := ADD(DE.STEMS(1), "os");
                OX(2) := ADD(DE.STEMS(1), "os");
                OX(3) := ADD(NULL_OX, "-");
              elsif DE.PART.ADJ.DECL.VAR = 8  then
                OX(1) := ADD(NULL_OX, "-");
                OX(2) := ADD(NULL_OX, "-");
                OX(3) := ADD(DE.STEMS(2), "on");
              end if;

            elsif DE.PART.ADJ.DECL.WHICH = 3  then
              if DE.PART.ADJ.DECL.VAR = 1  then
                OX(1) := ADD(DE.STEMS(1), "");
                OX(2) := ADD(NULL_OX, "(gen.)");
                OX(3) := ADD(DE.STEMS(2), "is");
              elsif DE.PART.ADJ.DECL.VAR = 2  then
                OX(1) := ADD(DE.STEMS(1), "is");
                OX(2) := ADD(DE.STEMS(2), "is");
                OX(3) := ADD(DE.STEMS(2), "e");
              elsif DE.PART.ADJ.DECL.VAR = 3  then
                OX(1) := ADD(DE.STEMS(1), "");
                OX(2) := ADD(DE.STEMS(2), "is");
                OX(3) := ADD(DE.STEMS(2), "e");
              end if;

            elsif DE.PART.ADJ.DECL.WHICH = 9  then
                OX(1) := ADD(DE.STEMS(1), "");
                OX(2) := ADD(NULL_OX, "undeclined");

            else
              raise NOT_FOUND;
            end if;

          elsif DE.PART.ADJ.CO = X    then
            if DE.PART.ADJ.DECL.WHICH = 1  then
              if DE.PART.ADJ.DECL.VAR = 1  then
                OX(1) := ADD(DE.STEMS(1), "us");
                OX(2) := ADD(DE.STEMS(2), "a -um");
                OX(3) := ADD(DE.STEMS(3), "or -or -us");
                OX(4) := ADD(DE.STEMS(4), "mus -a -um");
              elsif DE.PART.ADJ.DECL.VAR = 2  then
                OX(1) := ADD(DE.STEMS(1), "");
                OX(2) := ADD(DE.STEMS(2), "a -um");
                OX(3) := ADD(DE.STEMS(3), "or -or -us");
                OX(4) := ADD(DE.STEMS(4), "mus -a -um");
              end if;

            elsif DE.PART.ADJ.DECL.WHICH = 3  then
              if DE.PART.ADJ.DECL.VAR = 1  then
                OX(1) := ADD(DE.STEMS(1), "");
                OX(2) := ADD(DE.STEMS(2), "is (gen.)");
                OX(3) := ADD(DE.STEMS(3), "or -or -us");
                OX(4) := ADD(DE.STEMS(4), "mus -a -um");
              elsif DE.PART.ADJ.DECL.VAR = 2  then
                OX(1) := ADD(DE.STEMS(1), "is");
                OX(2) := ADD(DE.STEMS(2), "e");
                OX(3) := ADD(DE.STEMS(3), "or -or -us");
                OX(4) := ADD(DE.STEMS(4), "mus -a -um");
              elsif DE.PART.ADJ.DECL.VAR = 3  then
                OX(1) := ADD(DE.STEMS(1), "");
                OX(2) := ADD(DE.STEMS(2), "is -e");
                OX(3) := ADD(DE.STEMS(3), "or -or -us");
                OX(4) := ADD(DE.STEMS(4), "mus -a -um");
              end if;

            elsif DE.PART.ADJ.DECL.WHICH = 9  then
                OX(1) := ADD(DE.STEMS(1), "");
                OX(2) := ADD(NULL_OX, "undeclined");
                OX(3) := ADD(DE.STEMS(3), "or -or -us");
                OX(4) := ADD(DE.STEMS(4), "mus -a -um");

            else
              raise NOT_FOUND;
            end if;

          else
            raise NOT_FOUND;
          end if;


        elsif (DE.PART.PART = ADV) and then (DE.PART.ADV.CO = X)  then
           OX(1) := ADD(DE.STEMS(1), "");
           OX(2) := ADD(DE.STEMS(2), "");
           OX(3) := ADD(DE.STEMS(3), "");


        elsif DE.PART.PART = V    then
          if DE.PART.V.CON = (9, 9)  then
            OX(1) := ADD(DE.STEMS(1), "");
            OX(2) := ADD(NULL_OX, "undeclined");

          else

            if DE.PART.V.KIND = DEP  then
              OX(3) := ADD(NULL_OX, "-");
              OX(4) := ADD(DE.STEMS(4), "us sum");
              if DE.PART.V.CON.WHICH = 1  then
                OX(1) := ADD(DE.STEMS(1), "or");
                OX(2) := ADD(DE.STEMS(2), "ari");
              elsif DE.PART.V.CON.WHICH = 2  then
                OX(1) := ADD(DE.STEMS(1), "eor");
                OX(2) := ADD(DE.STEMS(2), "eri");
              elsif DE.PART.V.CON.WHICH = 3  then
                OX(1) := ADD(DE.STEMS(1), "or");
                --  Would be wrong for 3 3, but no 3 3 DEP
                if DE.PART.V.CON.VAR = 4  then
                  OX(2) := ADD(DE.STEMS(2), "iri");
                else
                  OX(2) := ADD(DE.STEMS(2), "i");
                end if;
              else
                raise NOT_FOUND;
              end if;


            else                            --  Not DEP 
              if DE.PART.V.KIND = IMPERS  then
                if DE.PART.V.CON.WHICH = 1  then
                  OX(1) := ADD(DE.STEMS(1), "at");
                elsif DE.PART.V.CON.WHICH = 2  then
                  OX(1) := ADD(DE.STEMS(1), "et");
                elsif DE.PART.V.CON.WHICH = 3  then
                  if DE.STEMS(1)(TRIM(DE.STEMS(1))'LAST) = 'i'  then
                    OX(1) := ADD(DE.STEMS(1), "t");
                  else
                    OX(1) := ADD(DE.STEMS(1), "it");
                  end if;
                end if;
              elsif DE.PART.V.CON.WHICH = 2  then
                OX(1) := ADD(DE.STEMS(1), "eo");

              elsif DE.PART.V.CON.WHICH = 5  then
                OX(1) := ADD(DE.STEMS(1), "um");
              elsif DE.PART.V.CON = (7, 2)  then
                OX(1) := ADD(DE.STEMS(1), "am");
              else
                OX(1) := ADD(DE.STEMS(1), "o");
              end if;                        --  OX(1) handled

              if DE.PART.V.CON.WHICH = 1  then
                OX(2) := ADD(DE.STEMS(2), "are");
              elsif DE.PART.V.CON.WHICH = 2  then
                OX(2) := ADD(DE.STEMS(2), "ere");
              elsif DE.PART.V.CON.WHICH = 3  then
                if DE.PART.V.CON.VAR = 2  then
                  OX(2) := ADD(DE.STEMS(2), "e");
                elsif DE.PART.V.CON.VAR = 3  then
                  OX(2) := ADD(DE.STEMS(2), "eri");
                elsif DE.PART.V.CON.VAR = 4  then
                  OX(2) := ADD(DE.STEMS(2), "ire");
                else
                  OX(2) := ADD(DE.STEMS(2), "ere");
                end if;
              elsif DE.PART.V.CON.WHICH = 5  then
                if DE.PART.V.CON.VAR = 1  then
                  OX(2) := ADD(DE.STEMS(2), "esse");
                elsif DE.PART.V.CON.VAR = 2  then
                  OX(2) := ADD(DE.STEMS(1), "e");  --  tricky, but it is 1
                end if;
              elsif DE.PART.V.CON.WHICH = 6  then
                if DE.PART.V.CON.VAR = 1  then
                  OX(2) := ADD(DE.STEMS(2), "re");
                elsif DE.PART.V.CON.VAR = 2  then
                  OX(2) := ADD(DE.STEMS(2), "le");
                end if;
              elsif DE.PART.V.CON.WHICH = 7  then
                if DE.PART.V.CON.VAR = 3  then
                  OX(2) := ADD(DE.STEMS(2), "se");
                end if;
              elsif DE.PART.V.CON.WHICH = 9  then
                if DE.PART.V.CON.VAR = 1  then
                  OX(2) := ADD(DE.STEMS(2), "are");
                elsif DE.PART.V.CON.VAR = 2  then
                  OX(2) := ADD(DE.STEMS(2), "ere");
                elsif DE.PART.V.CON.VAR = 3  then
                  OX(2) := ADD(DE.STEMS(2), "ere");
                elsif DE.PART.V.CON.VAR = 4  then
                  OX(2) := ADD(DE.STEMS(2), "ire");
                else
                  OX(2) := ADD(DE.STEMS(2), "ere");
                end if;
              end if;                        --  OX(2) handled


              if DE.PART.V.KIND = IMPERS  then
                OX(3) := ADD(DE.STEMS(1), "it");
              elsif DE.PART.V.CON = (5, 1)  then
                OX(3) := ADD(DE.STEMS(3), "i");
                OX(4) := ADD(DE.STEMS(4), "urus");
               elsif DE.PART.V.CON.WHICH = 9  then
                OX(3) := ADD("", "Additional");
                OX(4) := ADD("", "Forms");
               else
                OX(3) := ADD(DE.STEMS(3), "i");
                OX(4) := ADD(DE.STEMS(4), "us");
              end if;                         --  OX(3 & 4) handled

            end if;                 --  On V KIND

            if DE.PART.V.CON = (6, 1)  then      --  Finalization correction
                  OX(3) := ADD(OX(3), " (ii)");
            end if;
            if DE.PART.V.KIND = SEMIDEP  then    --  Finalization correction
                  OX(4) := ADD(DE.STEMS(4), "us sum");
            end if;

          end if;                        --  WHICH  ? 9

        elsif (DE.PART.PART = NUM) and then (DE.PART.NUM.KIND = X)  then
          if DE.PART.NUM.DECL.WHICH = 1  then
            if DE.PART.NUM.DECL.VAR = 1  then
              OX(1) := ADD(DE.STEMS(1), "us -a -um");
              OX(2) := ADD(DE.STEMS(2), "us -a -um");
              OX(3) := ADD(DE.STEMS(3), "i -ae -a");
              OX(4) := ADD(DE.STEMS(4), "");
            elsif DE.PART.NUM.DECL.VAR = 2  then
              OX(1) := ADD(DE.STEMS(1), "o -ae o");
              OX(2) := ADD(DE.STEMS(2), "us -a -um");
              OX(3) := ADD(DE.STEMS(3), "i -ae -a");
              OX(4) := ADD(DE.STEMS(4), "");
            elsif DE.PART.NUM.DECL.VAR = 3  then
              OX(1) := ADD(DE.STEMS(1), "es -es -ia");
              OX(2) := ADD(DE.STEMS(2), "us -a -um");
              OX(3) := ADD(DE.STEMS(3), "i -ae -a");
              OX(4) := ADD(DE.STEMS(4), "");
            elsif DE.PART.NUM.DECL.VAR = 4  then
              OX(1) := ADD(DE.STEMS(1), "i -ae -a");
              OX(2) := ADD(DE.STEMS(2), "us -a -um");
              OX(3) := ADD(DE.STEMS(3), "i -ae -a");
              OX(4) := ADD(DE.STEMS(4), "ie(n)s");
            end if;

            elsif DE.PART.NUM.DECL.WHICH = 2  then
              OX(1) := ADD(DE.STEMS(1), "");
              OX(2) := ADD(DE.STEMS(2), "us -a -um");
              OX(3) := ADD(DE.STEMS(3), "i -ae -a");
              OX(4) := ADD(DE.STEMS(4), "ie(n)s");

          end if;

        elsif (DE.PART.PART = NUM) and then (DE.PART.NUM.KIND = CARD)  then
          if DE.PART.NUM.DECL.WHICH = 1  then
            if DE.PART.NUM.DECL.VAR = 1  then
              OX(1) := ADD(DE.STEMS(1), "us");
              OX(2) := ADD(DE.STEMS(1), "a");
              OX(3) := ADD(DE.STEMS(1), "um");
            elsif DE.PART.NUM.DECL.VAR = 2  then
              OX(1) := ADD(DE.STEMS(1), "o");
              OX(2) := ADD(DE.STEMS(1), "ae");
              OX(3) := ADD(DE.STEMS(1), "o");
            elsif DE.PART.NUM.DECL.VAR = 3  then
              OX(1) := ADD(DE.STEMS(1), "es");
              OX(2) := ADD(DE.STEMS(1), "es");
              OX(3) := ADD(DE.STEMS(1), "ia");
            elsif DE.PART.NUM.DECL.VAR = 4  then
              OX(1) := ADD(DE.STEMS(1), "i");
              OX(2) := ADD(DE.STEMS(1), "ae");
              OX(3) := ADD(DE.STEMS(1), "a");
            end if;

          elsif DE.PART.NUM.DECL.WHICH = 2  then
              OX(1) := ADD(DE.STEMS(1), "");

          end if;

        elsif (DE.PART.PART = NUM) and then (DE.PART.NUM.KIND = ORD)  then
          OX(1) := ADD(DE.STEMS(1), "us");
          OX(2) := ADD(DE.STEMS(1), "a");
          OX(3) := ADD(DE.STEMS(1), "um");

        elsif (DE.PART.PART = NUM) and then (DE.PART.NUM.KIND = DIST)  then
          OX(1) := ADD(DE.STEMS(1), "i");
          OX(2) := ADD(DE.STEMS(1), "ae");
          OX(3) := ADD(DE.STEMS(1), "a");

        else
          OX(1) := ADD(DE.STEMS(1), "");
        end if;     -- On PART   


        if OX(1)(1..3) = "zzz"  then
          ADD_UP(" - ");
        elsif OX(1) /= NULL_OX  then
          ADD_UP(TRIM(OX(1)));
        end if;
        if OX(2)(1..3) = "zzz"  then
          ADD_UP(", - ");
        elsif OX(2) /= NULL_OX  then
          ADD_UP(", " & TRIM(OX(2)));
        end if;
        if OX(3)(1..3) = "zzz"  then
          ADD_UP(", - ");
        elsif OX(3) /= NULL_OX  then
          ADD_UP(", " & TRIM(OX(3)));
        end if;
        if OX(4)(1..3) = "zzz"  then
          ADD_UP(", - ");
        elsif OX(4) /= NULL_OX  then
          ADD_UP(", " & TRIM(OX(4)));
        end if;




      return TRIM(FORM);

    exception
      when NOT_FOUND  =>
        return "";
      when others     =>
        return "";
    end DICTIONARY_FORM;