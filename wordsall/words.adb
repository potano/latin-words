with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with STRINGS_PACKAGE; use STRINGS_PACKAGE;
with CONFIG; use CONFIG;
with WORD_PARAMETERS; use WORD_PARAMETERS;
with PARSE;
procedure WORDS is
  INPUT_LINE  : STRING(1..250) := (others => ' ');
begin
  CONFIGURATION := DEVELOPER_VERSION;

  if Ada.Command_Line.ARGUMENT_COUNT = 0  then      --  Simple WORDS
    METHOD := INTERACTIVE;
    SUPPRESS_PREFACE := FALSE;
    PARSE;

  elsif Ada.Command_Line.ARGUMENT_COUNT = 1  then      --  Simple WORDS
    ONE_ARGUMENT:
    declare
      INPUT_NAME  : constant STRING := TRIM(Ada.Command_Line.Argument(1));
    begin
      OPEN(INPUT, IN_FILE, INPUT_NAME);
      METHOD := COMMAND_LINE_FILES;
      SET_INPUT(INPUT);
      SUPPRESS_PREFACE := TRUE;
      PARSE;
    exception                  --  Triggers on INPUT
      when NAME_ERROR  =>
        METHOD := COMMAND_LINE_INPUT;            --  Found word in command line

    end ONE_ARGUMENT;

  elsif Ada.Command_Line.ARGUMENT_COUNT = 2  then    --  INPUT and OUTPUT files
    TWO_ARGUMENTS:
    declare
      INPUT_NAME  : constant STRING := TRIM(Ada.Command_Line.Argument(1));
      OUTPUT_NAME : constant STRING := TRIM(Ada.Command_Line.Argument(2));
    begin
      OPEN(INPUT, IN_FILE, INPUT_NAME);
      CREATE(OUTPUT, OUT_FILE, OUTPUT_NAME);
      METHOD := COMMAND_LINE_FILES;

      SET_INPUT(INPUT);
      SET_OUTPUT(OUTPUT);

      SUPPRESS_PREFACE := TRUE;
      OUTPUT_SCREEN_SIZE := INTEGER'LAST;
      PARSE;

      SET_INPUT(Ada.TEXT_IO.STANDARD_INPUT);
      SET_OUTPUT(Ada.TEXT_IO.STANDARD_OUTPUT);
      CLOSE(OUTPUT);
    exception                  --  Triggers on either INPUT or OUTPUT  !!!
      when NAME_ERROR  =>
        METHOD := COMMAND_LINE_INPUT;            --  Found words in command line

    end TWO_ARGUMENTS;
  else

    METHOD := COMMAND_LINE_INPUT;
  end if;


  if METHOD = COMMAND_LINE_INPUT  then            --  Process words in command line
    MORE_ARGUMENTS:
    begin
      SUPPRESS_PREFACE := TRUE;
      for I in 1..Ada.Command_Line.Argument_Count  loop  --  Assemble input words 
        INPUT_LINE := HEAD(TRIM(INPUT_LINE) & " " & Ada.Command_Line.Argument(I), 250);
      end loop;
      PARSE(TRIM(INPUT_LINE));
    end MORE_ARGUMENTS;
  end if;

end WORDS;
