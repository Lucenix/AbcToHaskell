{-# LANGUAGE QuasiQuotes, DeriveAnyClass, DeriveGeneric, TypeFamilies
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, FlexibleContexts, TemplateHaskell
    , DeriveDataTypeable, DeriveLift #-}
module AbcG4 where
import Language.ANTLR4
import Abc

fileHeader :: Information -> b -> Information -> [Either String Information] -> Key -> FileHeader
fileHeader a _ c d e = FileHeader (a:c:(Key e):(right d)) []

meterFraction :: Int -> Int -> Rational
meterFraction a b = fromIntegral a / fromIntegral b

tempo :: Rational -> Int -> (Maybe String, [Duration], Duration)
tempo a b = (Nothing, [Duration a], Duration (fromIntegral b))

key :: (PitchClass, Maybe Accidental) -> Maybe Mode -> Key
key (x,y) b = case b of
    Nothing -> Key_ (x,y,Major)
    Just c  -> Key_ (x,y,c)

pitch :: Maybe Accidental -> (PitchClass, Integer) -> [Int] -> Pitch
pitch a (p,o) c = Pitch (p, a, Octave (fromInteger o + sum c))

noteLength :: Int -> Int -> Duration
noteLength a b  = Duration (fromIntegral a / fromIntegral b)

abcTune :: Maybe [Information] -> [[Music]] -> Element
abcTune Nothing  b = Tune (AbcTune (TuneHeader []) (concat b))
abcTune (Just a) b = Tune (AbcTune (TuneHeader a)  (concat b))

abcFile :: FileHeader -> [Element] -> AbcFile
abcFile a b = AbcFile Nothing (Just a) b

a3 = (A,3)
a4 = (A,4)
b3 = (B,3)
b4 = (B,4)
c3 = (C,3)
c4 = (C,4)
d3 = (D,3)
d4 = (D,4)
e3 = (E,3)
e4 = (E,4)
f3 = (F,3)
f4 = (F,4)
g3 = (Abc.G,3)
g4_ = (Abc.G,4)
emptyList = []
sb   = Barline SingleBarline
dbff = Barline (DoubleBarline False False)
dbtf = Barline (DoubleBarline True False)
dbft = Barline (DoubleBarline False True)
rptf = Barline (Repeat 0 True False)
rpft = Barline (Repeat 0 False True)

left :: [Either a b] -> [a]
left [] = []
left ((Right h):hs) = left hs
left ((Left h):hs)  = h:left hs

right :: [Either a b] -> [b]
right [] = []
right ((Right h):hs) = h:right hs
right ((Left h):hs)  = right hs

$( return [] )

[g4|
    grammar Abc;
    abc : abc_header abc_music -> abcFile;
    abc_header : field_number comment* field_title other_fields* field_key -> fileHeader;

    field_number : 'X'':' NUMBER end_of_line -> ${\n _ -> ReferenceNumber (toInteger n)};
    field_title  : 'T'':' TEXT end_of_line   -> ${\t _ -> Title t};
    other_fields : field_composer            -> ${\a   -> Right (Composer a)}
                 | field_default_length      -> ${\a   -> Right (UnitNoteLength a)}
                 | field_meter               -> ${\a   -> Right (Meter a)}
                 | field_tempo               -> ${\a   -> Right (Tempo a)}
                 | field_voice               -> ${\a   -> Right (Voice a)}
                 | comment                   -> Left
                 ;

    field_composer       : 'C'':' TEXT end_of_line               -> ${\t _ -> t};
    field_default_length : 'L'':' note_length_strict end_of_line -> ${\d _ -> Duration d};
    field_meter          : 'M'':' meter end_of_line              -> ${\m _ -> m};
    field_tempo          : 'Q'':' tempo end_of_line              -> ${\t _ -> Tempo_ t};
    field_voice          : 'V'':' TEXT end_of_line               -> ${\t _ -> VoiceProperties (Just t) Nothing Nothing Nothing};
    field_key            : 'K'':' key end_of_line                -> ${\k _ -> k};

    key            : keynote mode_minor?      -> key;
    keynote        : basenote key_accidental? -> ${\(pc,_) acc -> (pc, acc)};
    key_accidental : '#' -> Sharp | 'b' -> Flat;
    mode_minor     : 'm' -> Minor;

    meter          : 'C'               -> Common
                   | 'C''|'            -> Cut
                   | meter_fraction    -> Simple
                   ;
    meter_fraction : NUMBER '/' NUMBER -> meterFraction;

    tempo : meter_fraction '=' NUMBER  -> tempo;

    abc_music : abc_tune+;
    abc_tune  : mid_tune_field? abc_line+ -> abcTune;
    abc_line  : element* NEWLINE          -> ${\l _ -> right l}
              | comment                   -> ${\_ -> []}
              ;

    element : note_element   -> Right
            | tuplet_element -> Right
            | barline        -> Right
            | NTH_REPEAT     -> Left
            | WHITESPACE     -> Left
            ;

    note_element : note
                 | multi_note -> ${\a -> Sequence a}
                 ;

    note         : note_or_rest note_length? -> ${\a b -> Chord (Chord_ (a,b))};
    note_or_rest : pitch+ | rest;

    pitch  : accidental? basenote octave* -> pitch;

    octave : COMMA      -> ${\_ -> -1}
           | APOSTROPHE -> ${\_ -> 1}
           ;

    note_length        : NUMBER ('/' NUMBER)? -> noteLength
                       | SLASHP               -> ${\l -> Duration (1/(2^(length l)))}
                       ;
    note_length_strict : NUMBER '/' NUMBER    -> meterFraction;

    accidental : '^'    -> Sharp
               | '^''^' -> DoubleSharp
               | '_'    -> Flat
               | '_''_' -> DoubleFlat
               | '='    -> Natural
               ;

    basenote : 'C' -> c3 | 'D' -> d3 | 'E' -> e3 | 'F' -> f3 | 'G' -> g3 | 'A' -> a3 | 'B' -> b3
             | 'c' -> c4 | 'd' -> d4 | 'e' -> e4 | 'f' -> f4 | 'g' -> g4_ | 'a' -> a4 | 'b' -> b4
             ;
    rest     : 'z' -> emptyList;

    tuplet_element : tuplet_spec note_element+ -> ${\a b -> Tuplet a (Sequence b) };
    tuplet_spec : '(' DIGIT -> ${\a -> Duration (fromIntegral a)};

    multi_note : '[' note+ ']';

    barline : '|'     -> sb
            | '|' '|' -> dbff
            | '[' '|' -> dbtf
            | '|' ']' -> dbft
            | ':' '|' -> rptf
            | '|' ':' -> rpft
            ;

    mid_tune_field : field_voice -> ${\v -> [Voice v]};

    comment : '%' TEXT NEWLINE -> ${\a _ -> a};
    end_of_line : comment | NEWLINE;

    NTH_REPEAT : '['['1''2']  -> String;
    APOSTROPHE : '\''         -> String;
    COMMA      : ','          -> String;
    SLASH      : '/'          -> String;
    SLASHP     : '/'+         -> String;
    TEXT       : (~[\n])+     -> String;
    NEWLINE    : [\n\r] '\n'? -> String;
    WHITESPACE : [' '\t]      -> String;
    DIGIT      : [0-9]        -> Int;
    NUMBER     : [0-9]+       -> Int;
|]
