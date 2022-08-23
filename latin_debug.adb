with TEXT_IO;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
pragma ELABORATE(DEVELOPER_PARAMETERS);
  package body LATIN_DEBUG is
    use TEXT_IO;
    use PART_OF_SPEECH_TYPE_IO;
    use DECN_RECORD_IO;
    use COMPARISON_TYPE_IO;
    use QUALITY_RECORD_IO;
    use INFLECTION_RECORD_IO;
    --use DICTIONARY_ENTRY_IO;
    use PARSE_RECORD_IO;      
    use DICT_IO;
    package BOOLEAN_IO is new TEXT_IO.ENUMERATION_IO(BOOLEAN);

  

    procedure PUT(ITEM : CHARACTER) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        TEXT_IO.PUT(DBG, ITEM);
      end if;
    end PUT;  

    procedure PUT(ITEM : STRING) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        TEXT_IO.PUT(DBG, ITEM);
      end if;
    end PUT;  

    procedure PUT_LINE(ITEM : STRING) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        TEXT_IO.PUT_LINE(DBG, ITEM);
      end if;
    end PUT_LINE;  

   procedure NEW_LINE(N : POSITIVE := 1) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        TEXT_IO.NEW_LINE(DBG, TEXT_IO.POSITIVE_COUNT(N));
      end if;
    end NEW_LINE;  

   
   procedure SET_COL(TO : POSITIVE) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        TEXT_IO.SET_COL(DBG, TEXT_IO.POSITIVE_COUNT(TO));
      end if;
    end SET_COL;   

   
    procedure PUT(ITEM : BOOLEAN) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        BOOLEAN_IO.PUT(DBG, ITEM);
      end if;
    end PUT;  

   procedure PUT(ITEM : INTEGER) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        INFLECTIONS_PACKAGE.INTEGER_IO.PUT(DBG, ITEM);
      end if;
    end PUT;  

   procedure PUT(ITEM : PART_OF_SPEECH_TYPE) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        PART_OF_SPEECH_TYPE_IO.PUT(DBG, ITEM);
      end if;
    end PUT;  

   procedure PUT(ITEM : QUALITY_RECORD) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        QUALITY_RECORD_IO.PUT(DBG, ITEM);
      end if;
    end PUT;  

   procedure PUT(ITEM : DECN_RECORD) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        DECN_RECORD_IO.PUT(DBG, ITEM);
      end if;
    end PUT;  

    procedure PUT(ITEM : COMPARISON_TYPE) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        COMPARISON_TYPE_IO.PUT(DBG, ITEM);
      end if;
    end PUT;  

    procedure PUT(ITEM : PRONOUN_KIND_TYPE) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        PRONOUN_KIND_TYPE_IO.PUT(DBG, ITEM);
      end if;
    end PUT;  

    procedure PUT(ITEM : AGE_TYPE) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        AGE_TYPE_IO.PUT(DBG, ITEM);
      end if;
    end PUT;  

    procedure PUT(ITEM : AREA_TYPE) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        AREA_TYPE_IO.PUT(DBG, ITEM);
      end if;
    end PUT;  

    procedure PUT(ITEM : PART_ENTRY) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        PART_ENTRY_IO.PUT(DBG, ITEM);
      end if;
    end PUT;  

    procedure PUT(ITEM : INFLECTION_RECORD) is 
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        INFLECTION_RECORD_IO.PUT(DBG, ITEM);
      end if;
    end PUT;  

    --procedure PUT(ITEM : DICTIONARY_ENTRY) is 
    --    begin 
    --      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
    --        DICTIONARY_ENTRY_IO.PUT(DBG, ITEM);
    --      end if;
    --    end PUT;  

    procedure PUT(ITEM : DICTIONARY_KIND) is
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        TEXT_IO.PUT(DBG, EXT(ITEM));
      end if;
    end PUT;  

    procedure PUT(ITEM : PARSE_RECORD) is
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        PARSE_RECORD_IO.PUT(DBG, ITEM);
      end if;
    end PUT;  

    procedure PUT(ITEM : DICT_IO.POSITIVE_COUNT) is
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        if ITEM = NULL_MNPC  then
          PUT(DBG, "     0");
        else
          INFLECTIONS_PACKAGE.INTEGER_IO.PUT(DBG, INTEGER(ITEM), 6);
        end if;
      end if;
    end PUT;  

    procedure PUT(ITEM : SUFFIX_ENTRY) is
    begin 
      if WORDS_MDEV(WRITE_DEBUG_FILE)     then
        SUFFIX_ENTRY_IO.PUT(DBG, ITEM);
      end if;
    end PUT;  
 

end LATIN_DEBUG;
