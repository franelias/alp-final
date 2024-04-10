{
module Parse where
}

%name parseComms
%tokentype { Token }
%error { parseError }

%token
    ReferenceDuration { TReferenceD }
    Title             { TTitle }
    Repeat            { TRepeat }
    Invert            { TInvert }
    Transpose         { TTranpose }
    Seed              { TSeed }
    Melody            { TMelody }
    Chord             { TChord }
    Rest              { TRest }
    Note              { TNote }
    Int               { TInt $$ }
    Float             { TFloat $$ }
    String            { TString $$ }
    ';'               { TSemicolon }
    '('               { TOpen }
    ')'               { TClose }
    '+'               { TPlus }
    '-'               { TMinus }
    '*'               { TTimes }
    '/'               { TDiv }
    ':+:'             { TConcat }
    ':|:'             { TConcatD }
    '#'               { TSharp }
    'b'               { TFlat }
    ','               { TComma }
    '.'               { TDot }

%%

comm    :  comm ';' comm             { Semicolon $1 $3 }
        |  ReferenceDuration factor  { ReferenceD $2 }
        |  Title factor              { Title $2 }
        |  Repeat factor             { Repeat $2 }
        |  Invert factor             { Invert }
        |  Transpose factor          { Transpose $2 }
        |  Seed factor               { Seed $2 }
        |  Melody melody             { Melody $2 }

melody  :  melody ':+:' melody            { Concat $1 $3 }
        |  melody ':|:' melody            { ConcatD $1 $3 }
        |  Chord '(' notes ',' factor ')' { Chord $3 $5 }
        |  Rest '(' factor ')'            { Rest $3 $5 }

notes   :  note ',' note    { Comma $1 $3 }
        |  note             { $1 } 

note    :  factor modifier octave { Note $1 $2 $3 }

factor  :  Int     { Int $1 }
        |  Float   { Float $1 }
        |  String  { String $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Comm
    = Semicolon Comm Comm
    | ReferenceDuration Double
    | Title String
    | Repeat Int
    | Invert
    | Transpose Int
    | Seed Int
    | Melody Melody
    deriving Show

data Melody
    = Concat Melody Melody
    | ConcatD Melody Melody
    | Chord Notes Double
    | Rest Double
    deriving Show

data Notes
    = Comma Notes Notes
    | ConcatD Melody Melody
    | Chord Notes Double
    | Rest Double
    deriving Show

data Factor
    = Int Int
    | Var String
    | Brack Exp
    deriving Show

}