   with TEXT_IO; use TEXT_IO;
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;  
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   with LINE_STUFF; use LINE_STUFF;
   procedure LINEWHIT is 
      package INTEGER_IO is new TEXT_IO.INTEGER_IO(INTEGER);
      use DICTIONARY_ENTRY_IO;
      use PART_ENTRY_IO;
      use KIND_ENTRY_IO;
      use AGE_TYPE_IO;
      use AREA_TYPE_IO;
      use GEO_TYPE_IO;
      use FREQUENCY_TYPE_IO;
      use SOURCE_TYPE_IO;
   
      DE : DICTIONARY_ENTRY;
   
      WHITE_FILE : FILE_TYPE;
      OUTPUT : FILE_TYPE;
   
      ST : STEM_TYPE := NULL_STEM_TYPE;
      BLK_STEM : constant STEM_TYPE := NULL_STEM_TYPE;
      STS : STEMS_TYPE := NULL_STEMS_TYPE;
      MEAN : MEANING_TYPE := NULL_MEANING_TYPE;
      PT  : PART_ENTRY  := NULL_PART_ENTRY;
   
      LINE, OUTLINE, BLANK_LINE : STRING(1..300) := 
         (others => ' ');
      L, LAST : INTEGER := 0;
      NUMBER_OF_DICTIONARY_ENTRIES : INTEGER := 0;
   
     
   
   
   begin
      PUT_LINE("LINEWHIT.IN (White's  - 2 lines) -> LINEWHIT.OUT (DICTLINE format)");
   
      CREATE(OUTPUT, OUT_FILE, "LINEWHIT.OUT");
   
      OPEN(WHITE_FILE, IN_FILE, "LINEWHIT.IN");
     
   
   
      
         
            while not END_OF_FILE(WHITE_FILE)  loop
              LINE    := BLANK_LINE;
              OUTLINE := BLANK_LINE;
          
              GET_NON_COMMENT_LINE(WHITE_FILE, LINE, LAST);      
    --PUT_LINE(INTEGER'IMAGE(LAST) & "=="& LINE(1..LAST));
              
              OUTLINE(1..LAST) := LINE(1..LAST);
            
              LINE := BLANK_LINE;
              GET_NON_COMMENT_LINE(WHITE_FILE, LINE, LAST);    
              OUTLINE(101..100+LAST) := LINE(1..LAST);
            
              PUT_LINE(OUTPUT, OUTLINE(1..190));
            
              NUMBER_OF_DICTIONARY_ENTRIES := NUMBER_OF_DICTIONARY_ENTRIES + 1;

            end loop;
            
                              
           
         
           
      
     
     
      CLOSE(OUTPUT);
      SET_COL(33); PUT("--  "); INTEGER_IO.PUT(NUMBER_OF_DICTIONARY_ENTRIES);
      PUT(" entries");    SET_COL(55); PUT_LINE("--  Loaded correctly");
   end LINEWHIT;

