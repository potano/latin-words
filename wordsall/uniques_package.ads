with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
package UNIQUES_PACKAGE is
  
  type UNIQUE_ITEM;
  type UNIQUE_LIST is access UNIQUE_ITEM;

  type UNIQUE_ITEM is
    record
      PR   : PARSE_RECORD;
      SUCC : UNIQUE_LIST;
    end record;

  type LATIN_UNIQUES is array (CHARACTER range 'a'..'z') of UNIQUE_LIST;
  NULL_LATIN_UNIQUES : LATIN_UNIQUES := (others => null);

  UNQ : LATIN_UNIQUES := NULL_LATIN_UNIQUES;

  
end UNIQUES_PACKAGE;