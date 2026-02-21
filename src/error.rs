use crate::constants::*;
use crate::datastructures::{
    equiv, r#box, box_mut, font_id_text, mag, tracing_online
};
use crate::io::term_input_string;
use crate::math::fam;
use crate::{
    Global, HalfWord, Integer, QuarterWord, Scaled, StrNum, update_terminal
};

use std::io::Write;

// Part 6: Reporting errors

pub enum TeXError {
    Arith,
    Overflow(&'static str, Integer),
    Confusion(&'static str),
    IO(&'static str),
    Fatal(&'static str),
    
    // Other errors that do no stop original TeX,
    // but will stop this implementation.
    // Section 288
    IncompatibleMag,
    IllegalMag(Integer),
    // Section 336
    IncompleteIf,
    IncompleteIfForbidden,
    // Section 338
    FileEndedOrForbiddenCSFound,
    // Section 346
    InvalidCharacter,
    // Section 370
    UndefinedControlSequence,
    // Section 373
    MissingEncCSName,
    // Section 395
    ArgumentExtraRightBrace,
    // Section 396
    ParagraphEndedBefore,
    // Section 398
    DoesNotMatchDefinition,
    // Section 403
    MissingLeftBrace,
    // Section 408
    IncompatibleGlueUnits,
    // Section 415, 446
    MissingNumber,
    // Section 418
    ImproperMode(Integer),
    // Section 428
    CantUseAfterThe,
    // Section 433
    BadRegisterCode(Integer),
    // Section 434
    BadCharacterCode(Integer),
    // Section 435
    BadNumber(Integer),
    // Section 436
    BadMathChar(Integer),
    // Section 437
    BadDelimiterCode(Integer),
    // Section 442
    ImproperAlphabeticConstant,
    // Section 445
    NumberTooBig,
    // Section 454
    IllegalUnitOfMeasureFilll,
    // Section 456
    IllegalUnitOfMeasureMu,
    // Section 459
    IllegalUnitOfMeasurePt,
    // Section 460
    DimensionTooLarge,
    // Section 475
    MissingLeftBrace2,
    // Section 476
    AlreadyNineParameters,
    ParametersNumberedConsecutively,
    // Section 479
    IllegalParameterNumber,
    // Section 486
    FileEndedWithin,
    // Section 500
    ExtraOr,
    // Section 503
    MissingEqual(HalfWord),
    // Section 510
    ExtraFiOrElse,
    // Section 530
    CantFindFile,
    CantWriteFile,
    // Section 561
    TfmNotLoadable(bool, HalfWord, Scaled),
    // Section 567
    TfmNotLoaded(HalfWord, Scaled),
    // Section 577
    MissingFontIdentifier,
    // Section 579
    FontHasOnly(QuarterWord),
    // Section 641
    HugePage,
    // Section 723
    UndefinedCharacter(HalfWord),
    // Section 776
    ImproperHalignDisplay,
    // Section 783
    MissingCroisillonAlign,
    // Section 784
    OnlyOneCroisillonAllowed,
    // Section 792
    ExtraAlignmentTab,
    // Section 826
    InfiniteGlueShrinkageInParagraph,
    // Section 936
    ImproperHyphenation,
    // Section 937
    NotALetter,
    // Section 960
    TooLateForPatterns,
    // Section 961
    BadPatterns,
    // Section 962
    Nonletter,
    // Section 963
    DuplicatePattern,
    // Section 976
    InfiniteGlueShrinkageInBoxBeingSplit,
    // Section 978
    VsplitNeedsAVbox,
    // Section 993
    InsertionCanOnlyBeAddedToVbox(HalfWord),
    // Section 1004
    InfiniteGlueShrinkageOnCurrentPage,
    // Section 1009
    InfiniteGlueShrinkageInsertedFrom(Integer),
    // Section 1015
    Box255IsNotVoid,
    // Section 1024
    OutputLoop,
    // Section 1027
    UnbalancedOutputRoutine,
    // Section 1028
    OutputRoutineDidntUseAllOfBox255,
    // Section 1049, 1050
    ReportIllegalCase,
    // Section 1047
    MissingDollar,
    // Section 1064, 1065
    MissingEndGroup,
    MissingMathRight,
    MissingRightBrace,
    // Section 1066
    Extra,
    // Section 1068
    TooManyRightBraces,
    // Section 1069
    ExtraRightBraceOrForgotten,
    // Section 1078
    LeadersNotFollowedByProperGlue,
    // Section 1080
    CantUseIn,
    CantUseIn2,
    LastBoxVoidMath,
    LastBoxVoidVmode,
    // Section 1082
    MissingTo,
    // Section 1084
    BoxWasSupposedToBeHere,
    // Section 1095
    CantUseHrule,
    // Section 1099
    CantInsert255,
    // Section 1106
    CantTakeThings,
    // Section 1110
    IncompatibleListCantBeUnboxed,
    // Section 1120
    IllegalMathDisc,
    DiscListTooLong,
    // Section 1121
    ImproperDiscList,
    // Section 1127
    MissingLeftBrace3,
    MissingRightBrace2,
    // Section 1128
    MisplacedTabMark,
    // Section 1129
    MisplacedNoalign,
    MisplacedOmit,
    // Section 1132
    MissingCr,
    // Section 1135
    ExtraEndcsname,
    // Section 1159
    LimitControlsMustFollowMathOp,
    // Section 1161
    MissingDelimiterLeftParen,
    // Section 1166
    UseMathAccentInMathMode,
    // Section 1177
    DoubleSuperscript,
    DoubleSubscript,
    // Section 1183
    AmbiguousFraction,
    // Section 1192
    ExtraMathRight,
    // Section 1195
    InsufficientSymbolFonts,
    InsufficientExtensionFonts,
    // Section 1197
    DisplayMathEndsWithDollars,
    // Section 1207
    MissingDollarDollar,
    // Section 1212
    CantUsePrefix,
    // Section 1213
    CantUseLongOuter,
    // Section 1215
    MissingControlSequence,
    // Section 1225
    MissingTo2,
    // Section 1232
    InvalidCode(Integer, HalfWord),
    // Section 1237
    CantUseAfterCmd(QuarterWord),
    // Section 1241
    ImproperSetbox,
    // Section 1243
    BadSpaceFactor,
    // Section 1244
    BadPrevGraf,
    // Section 1252
    PatternsOnlyIniTeX,
    // Section 1259
    ImproperAt(Integer),
    // Section 1283
    ErrMessage(StrNum),
    // Section 1293
    ShowWhatever,
    // Section 1304
    CantDumpInGroup,
    // Section 1372
    UnbalancedWriteCmd,

    // Format
    CantFindFormat,
}

pub(crate) type TeXResult<T> = Result<T, TeXError>;

macro_rules! help_lines {
    ($($lines:expr),*) => {
        vec![$($lines),*]
    };
}

impl Global {
    // Section 92
    pub(crate) fn normalize_selector(&mut self) -> TeXResult<()> {
        self.selector = if self.log_opened {
            TERM_AND_LOG
        }
        else {
            TERM_ONLY
        };

        if self.job_name == 0 {
            self.open_log_file()?;
        }
        if self.interaction == BATCH_MODE {
            self.selector -= 1;
        }
        Ok(())
    }

    // Section 96
    pub(crate) fn check_interrupt(&mut self) -> TeXResult<()> {
        if !self.interrupt || !self.ok_to_interrupt {
            return Ok(());
        }

        // Section 98
        self.interaction = ERROR_STOP_MODE;
        if self.selector == LOG_ONLY || self.selector == NO_PRINT {
            self.selector += 1;
        }
        println!();
        println!("! Interruption.");
        println!("You rang?");
        println!("Not supported at the moment.");
        println!("Type <return> to continue or X to quit.");
        print!("? ");
        update_terminal!();
        if term_input_string()?.trim() == "X" {
            Err(TeXError::Fatal("interrupted by the user"))
        }
        else {
            Ok(())
        }
    }

    // Section 327: back_error - back_input then error
    pub(crate) fn back_error(&mut self, texerror: TeXError) -> TeXResult<()> {
        self.ok_to_interrupt = false;
        self.back_input()?;
        self.ok_to_interrupt = true;
        self.error(texerror)
    }

    // Section 327: ins_error - back_input then error
    // Unlike back_error, ins_error sets token_type to INSERTED
    pub(crate) fn ins_error(&mut self, texerror: TeXError) -> TeXResult<()> {
        self.ok_to_interrupt = false;
        self.back_input()?;
        *self.token_type_mut() = INSERTED;
        self.ok_to_interrupt = true;
        self.error(texerror)
    }

    // Section 992
    fn box_error(&mut self, n: HalfWord) -> TeXResult<()> {
        self.begin_diagnostic();
        self.print_nl("The following box has been deleted:");
        self.show_box(r#box(n));
        self.end_diagnostic(true);
        self.flush_node_list(r#box(n))?;
        *box_mut(n) = NULL;
        Ok(())
    }

    // Section 82
    pub fn error(&mut self, texerror: TeXError) -> TeXResult<()> {
        // Section 73
        macro_rules! print_err {
            ($s:expr) => {
                {
                    self.print_nl("! ");
                    self.print($s);
                }
            };
        }
        
        // Section 1049
        macro_rules! you_cant {
            () => {
                print_err!("You can't use `");
                self.print_cmd_chr(self.cur_cmd, self.cur_chr);
                self.print("' in ");
                self.print_mode(self.mode());
            };
        }

        let help_message = match texerror {
            TeXError::IO(s) => {
                self.selector = TERM_ONLY;
                print_err!("Input/output error(");
                self.print(s);
                self.print_char(b')');
                help_lines!(
                    "Something wrong manipulating file happened.",
                    "By precaution, this message is only printed in terminal."
                )
            },

            // Section 93
            TeXError::Fatal(s) => {
                self.normalize_selector()?;
                print_err!("Emergency stop");
                help_lines!(s)
            },

            // Section 94
            TeXError::Overflow(s, n) => {
                self.normalize_selector()?;
                print_err!("TeX capacity exceeded, sorry [");
                self.print(s);
                self.print_char(b'=');
                self.print_int(n);
                self.print_char(b']');
                help_lines!(
                    "If you really absolutely need more capacity",
                    "you can ask a wizard to enlarge me."
                )
            },

            // Section 95
            TeXError::Confusion(s) => {
                self.normalize_selector()?;
                print_err!("This can't happen (");
                self.print(s);
                self.print_char(b')');
                help_lines!("I'm broken. Please show this to someone who can fix can fix.")
            },

            // Section 288
            TeXError::IncompatibleMag => {
                print_err!("Incompatible magnification (");
                self.print_int(mag());
                self.print(");");
                self.print_nl(" the previous value will be retained");
                self.print(" (");
                self.print_int(self.mag_set);
                self.print(")");
                help_lines!(
                    "I can handle only one magnification ratio per job. So I've",
                    "reverted to the magnification you used earlier on this run."
                )
            },

            TeXError::IllegalMag(bad_mag) => {
                print_err!("Illegal magnification has been changed to 1000");
                self.print(" (");
                self.print_int(bad_mag);
                self.print(")");
                help_lines!("The magnification ratio must be between 1 and 32768.")
            },

            // Section 336
            // Message is printed by check_outer_validity before calling ins_error
            TeXError::IncompleteIf => {
                help_lines!(
                    "The file ended while I was skipping conditional text.",
                    "This kind of error happens when you say `\\if...' and forget",
                    "the matching `\\fi'. I've inserted a `\\fi'; this might work."
                )
            },

            TeXError::IncompleteIfForbidden => {
                help_lines!(
                    "A forbidden control sequence occurred in skipped text.",
                    "This kind of error happens when you say `\\if...' and forget",
                    "the matching `\\fi'. I've inserted a `\\fi'; this might work."
                )
            },

            // Section 338
            // Message is printed by check_outer_validity before calling error
            TeXError::FileEndedOrForbiddenCSFound => {
                help_lines!(
                    "I suspect you have forgotten a `}', causing me",
                    "to read past where you wanted me to stop.",
                    "I'll try to recover; but if the error is serious,",
                    "you'd better type `E' or `X' now and fix your file."
                )
            },

            // Section 346
            TeXError::InvalidCharacter => {
                print_err!("Text line contains an invalid character");
                help_lines!(
                    "A funny symbol that I can't read has just been input.",
                    "Continue, and I'll forget that it ever happened."
                )
            },

            // Section 370
            TeXError::UndefinedControlSequence => {
                print_err!("Undefined control sequence");
                help_lines!(
                    "The control sequence at the end of the top line",
                    "of your error message was never \\def'ed. If you have",
                    "misspelled it (e.g., `\\hobx'), type `I' and the correct",
                    "spelling (e.g., `I\\hbox'). Otherwise just continue,",
                    "and I'll forget about whatever was undefined."
                )
            },

            // Section 373
            TeXError::MissingEncCSName => {
                print_err!("Missing ");
                self.print_esc("endcsname");
                help_lines!(
                    "The control sequence marked <to be read again> should",
                    "not appear between \\csname and \\endcsname."
                )
            },

            // Section 395
            TeXError::ArgumentExtraRightBrace => {
                print_err!("Argument of ");
                self.sprint_cs(self.warning_index);
                self.print(" has an extra }");
                help_lines!(
                    "I've run across a `}' that doesn't seem to match anything.",
                    "For example, `\\def\\a#1{...}' and `\\a}' would produce",
                    "this error. If you simply proceed now, the `\\par' that",
                    "I've just inserted will cause me to report a runaway",
                    "argument that might be the root of the problem. But if",
                    "your `}' was spurious, just type `2' and it will go away."
                )
            },

            // Section 396
            TeXError::ParagraphEndedBefore => {
                print_err!("Paragraph ended before ");
                self.sprint_cs(self.warning_index);
                self.print(" was complete");
                help_lines!(
                    "I suspect you've forgotten a `}', causing me to apply this",
                    "control sequence to too much text. How can we recover?",
                    "My plan is to forget the whole thing and hope for the best."
                )
            },

            // Section 398
            TeXError::DoesNotMatchDefinition => {
                print_err!("Use of ");
                self.sprint_cs(self.warning_index);
                self.print(" doesn't match its definition");
                help_lines!(
                    "If you say, e.g., `\\def\\a1{...}', then you must always",
                    "put `1' after `\\a', since control sequence names are",
                    "made up of letters only. The macro here has not been",
                    "followed by the required stuff, so I'm ignoring it."
                )
            },

            // Section 403
            TeXError::MissingLeftBrace => {
                print_err!("Missing { inserted");
                help_lines!(
                    "A left brace was mandatory here, so I've put one in.",
                    "You might want to delete and/or insert some corrections",
                    "so that I will find a matching right brace soon.",
                    "(If you're confused by all this, try typing `I}' now.)"
                )
            },

            // Section 408
            TeXError::IncompatibleGlueUnits => {
                print_err!("Incompatible glue units");
                help_lines!("I'm going to assume that 1mu=1pt when they're mixed.")
            },

            // Section 415, 446
            TeXError::MissingNumber => {
                print_err!("Missing number, treated as zero");
                help_lines!(
                    "A number should have been here; I inserted `0'.",
                    "(If you can't figure out why I needed to see a number,",
                    "look up `weird error' in the index to The TeXbook.)"
                )
            },

            // Section 418
            TeXError::ImproperMode(m) => {
                print_err!("Improper ");
                self.print_cmd_chr(SET_AUX, m);
                help_lines!(
                    "You can refer to \\spacefactor only in horizontal mode;",
                    "you can refer to \\prevdepth only in vertical mode; and",
                    "neither of these is meaningful inside \\write. So",
                    "I'm forgetting what you said and using zero instead."
                )
            },

            // Section 428
            TeXError::CantUseAfterThe => {
                print_err!("You can't use `");
                self.print_cmd_chr(self.cur_cmd, self.cur_chr);
                self.print("' after ");
                self.print_esc("the");
                help_lines!("I'm forgetting what you said and using zero instead.")
            },

            // Section 433
            TeXError::BadRegisterCode(n) => {
                print_err!("Bad register code");
                self.print(" (");
                self.print_int(n);
                self.print(")");
                help_lines!(
                    "A register number must be between 0 and 255.",
                    "I changed this one to zero."
                )
            },

            // Section 434
            TeXError::BadCharacterCode(n) => {
                print_err!("Bad character code");
                self.print(" (");
                self.print_int(n);
                self.print(")");
                help_lines!(
                    "A character number must be between 0 and 255.",
                    "I changed this one to zero."
                )
            },

            // Section 435
            TeXError::BadNumber(n) => {
                print_err!("Bad number");
                self.print(" (");
                self.print_int(n);
                self.print(")");
                help_lines!(
                    "Since I expected to read a number between 0 and 15,",
                    "I changed this one to zero."
                )
            },

            // Section 436
            TeXError::BadMathChar(n) => {
                print_err!("Bad mathchar");
                self.print(" (");
                self.print_int(n);
                self.print(")");
                help_lines!(
                    "A mathchar number must be between 0 and 32767.",
                    "I changed this one to zero."
                )
            },

            // Section 437
            TeXError::BadDelimiterCode(n) => {
                print_err!("Bad delimiter code");
                self.print(" (");
                self.print_int(n);
                self.print(")");
                help_lines!(
                    "A numeric delimiter code must be between 0 and 2^{27}-1.",
                    "I changed this one to zero."
                )
            },

            // Section 442
            TeXError::ImproperAlphabeticConstant => {
                print_err!("Improper alphabetic constant");
                help_lines!(
                    "A one-character control sequence belongs after a ` mark.",
                    "So I'm essentially inserting \\0 here."
                )
            },

            // Section 445
            TeXError::NumberTooBig => {
                print_err!("Number too big");
                help_lines!(
                    "I can only go up to 2147483647='17777777777=\"7FFFFFFF,",
                    "so I'm using that number instead of yours."
                )
            },

            // Section 454
            TeXError::IllegalUnitOfMeasureFilll => {
                print_err!("Illegal unit of measure (");
                self.print("replaced by filll)");
                help_lines!("I dddon't go any higher than filll.")
            },

            // Section 456
            TeXError::IllegalUnitOfMeasureMu => {
                print_err!("Illegal unit of measure (");
                self.print("mu inserted)");
                help_lines!(
                    "The unit of measurement in math glue must be mu.",
                    "To recover gracefully from this error, it's best to",
                    "delete the erroneous units; e.g., type `2' to delete",
                    "two letters. (See Chapter 27 of The TeXbook.)"
                )
            },

            // Section 459
            TeXError::IllegalUnitOfMeasurePt => {
                print_err!("Illegal unit of measure (");
                self.print("pt inserted)");
                help_lines!(
                    "Dimensions can be in units of em, ex, in, pt, pc,",
                    "cm, mm, dd, cc, bp, or sp; but yours is a new one!",
                    "I'll assume that you meant to say pt, for printer's points.",
                    "To recover gracefully from this error, it's best to",
                    "delete the erroneous units; e.g., type `2' to delete",
                    "two letters. (See Chapter 27 of The TeXbook.)"
                )
            },

            // Section 460
            TeXError::DimensionTooLarge => {
                print_err!("Dimension too large");
                help_lines!(
                    "I can't work with sizes bigger than about 19 feet.",
                    "Continue and I'll use the largest value I can."
                )
            },

            // Section 475
            TeXError::MissingLeftBrace2 => {
                print_err!("Missing { inserted");
                help_lines!(
                    "Where was the left brace? You said something like `\\def\\a}',",
                    "which I'm going to interpret as `\\def\\a{}'."
                )
            },

            // Section 476
            TeXError::AlreadyNineParameters => {
                print_err!("You already have nine parameters");
                help_lines!(
                    "I'm going to ignore the # sign you just used,",
                    "as well as the token that followed it."
                )
            },
            TeXError::ParametersNumberedConsecutively => {
                print_err!("Parameters must be numbered consecutively");
                help_lines!(
                    "I've inserted the digit you should have used after the #.",
                    "Type `1' to delete what you did use."
                )
            },

            // Section 479
            TeXError::IllegalParameterNumber => {
                print_err!("Illegal parameter number in definition of ");
                self.sprint_cs(self.warning_index);
                help_lines!(
                    "You meant to type ## instead of #, right?",
                    "Or maybe a } was forgotten somewhere earlier, and things",
                    "are all screwed up? I'm going to assume that you meant ##."
                )
            },

            // Section 486
            TeXError::FileEndedWithin => {
                print_err!("File ended within ");
                self.print_esc("read");
                help_lines!("This \\read has unbalanced braces.")
            },

            // Section 500
            TeXError::ExtraOr => {
                print_err!("Extra ");
                self.print_esc("or");
                help_lines!("I'm ignoring this; it doesn't match any \\if.")
            },

            // Section 503
            TeXError::MissingEqual(this_if) => {
                print_err!("Missing = inserted for ");
                self.print_cmd_chr(IF_TEST, this_if);
                help_lines!("I was expecting to see `<', `=', or `>'. Didn't.")
            },

            // Section 510
            TeXError::ExtraFiOrElse => {
                print_err!("Extra ");
                self.print_cmd_chr(FI_OR_ELSE, self.cur_chr);
                help_lines!("I'm ignoring this; it doesn't match any \\if.")
            },

            // Section 530
            TeXError::CantFindFile => {
                print_err!("I can't find file '");
                self.print_file_name(self.cur_name, self.cur_area, self.cur_ext);
                self.print_char(b'\'');
                help_lines!("Maybe you have mispelled the name.")
            },

            TeXError::CantWriteFile => {
                print_err!("I can't write on file '");
                self.print_file_name(self.cur_name, self.cur_area, self.cur_ext);
                help_lines!("Is this file already busy?")
            },

            // Section 561
            TeXError::TfmNotLoadable(file_opened, u, s) => {
                print_err!("Font ");
                self.sprint_cs(u);
                self.print_char(b'=');
                self.print_strnumber(self.cur_area);
                self.print_strnumber(self.cur_name);
                if s >= 0 {
                    self.print(" at ");
                    self.print_scaled(s);
                    self.print("pt");
                }
                else if s != -1000 {
                    self.print(" scaled ");
                    self.print_int(-s);
                }
                self.print(
                    match file_opened {
                        true => " not loadable: Bad metric (TFM) file",
                        false => " not loadable: Metric (TFM) file not found"
                    }
                );
                help_lines!(
                    "I wasn't able to read the size data for this font,",
                    "so I will ignore the font specification.",
                    "[Wizards can fix TFM files using TFtoPL/PLtoTF.]",
                    "You might try inserting a different font spec;",
                    "e.g., type `I\\font<same font id>=<substitute font name>'."
                )
            },

            // Section 567
            TeXError::TfmNotLoaded(u, s) => {
                print_err!("Font ");
                self.sprint_cs(u);
                self.print_char(b'=');
                self.print_strnumber(self.cur_area);
                self.print_strnumber(self.cur_name);
                if s >= 0 {
                    self.print(" at ");
                    self.print_scaled(s);
                    self.print("pt");
                }
                else if s != -1000 {
                    self.print(" scaled ");
                    self.print_int(-s);
                }
                print_err!(" not loaded: Not enough room left");
                help_lines!(
                    "I'm afraid I won't be able to make use of this font,",
                    "because my memory for character-size data is too small.",
                    "If you're really stuck, ask a wizard to enlarge me.",
                    "Or maybe try `I\\font<same font id>=<name of loaded font>'."
                )
            },

            // Section 577
            TeXError::MissingFontIdentifier => {
                print_err!("Missing font identifier");
                help_lines!(
                    "I was looking for a control sequence whose",
                    "current meaning has been defined by \\font."
                )
            },

            // Section 579
            TeXError::FontHasOnly(f) => {
                print_err!("Font ");
                self.print_esc_strnumber(font_id_text(f) as StrNum);
                self.print(" has only ");
                self.print_int(self.font_params[f as usize] as Integer);
                self.print(" fontdimen parameters");
                help_lines!(
                    "To increase the number of font parameters, you must",
                    "use \\fontdimen immediately after the \\font is loaded."
                )
            },

            // Section 641
            TeXError::HugePage => {
                print_err!("Huge page cannot be shipped out");
                help_lines!(
                    "The page just created is more than 18 feet tall or",
                    "more than 18 feet wide, so I suspect something went wrong."
                )
            },

            // Section 723
            TeXError::UndefinedCharacter(a) => {
                print_err!("");
                self.print_size(self.cur_size as Integer);
                self.print_char(b' ');
                self.print_int(fam(a) as Integer);
                self.print(" is undefined (character ");
                self.print_strnumber(self.cur_c as StrNum);
                self.print(")");
                help_lines!(
                    "Somewhere in the math formula just ended, you used the",
                    "stated character from an undefined font family. For example,",
                    "plain TeX doesn't allow \\it or \\sl in subscripts. Proceed,",
                    "and I'll try to forget that I needed that character."
                )
            },

            // Section 776
            TeXError::ImproperHalignDisplay => {
                print_err!("Improper ");
                self.print_esc("halign inside $$'s");
                help_lines!(
                    "Displays can use special alignments (like \\eqalignno)",
                    "only if nothing but the alignment itself is between $$'s.",
                    "So I've deleted the formulas that preceded this alignment."
                )
            },

            // Section 783
            TeXError::MissingCroisillonAlign => {
                print_err!("Missing # inserted in alignment preamble");
                help_lines!(
                    "There should be exactly one # between &'s, when an",
                    "\\halign or \\valign is being set up. In this case you had",
                    "none, so I've put one in; maybe that will work."
                )
            },

            // Section 784
            TeXError::OnlyOneCroisillonAllowed => {
                print_err!("Only one # is allowed per tab");
                help_lines!(
                    "There should be exactly one # between &'s, when an",
                    "\\halign or \\valign is being set up. In this case you had",
                    "more than one, so I'm ignoring all but the first."
                )
            },

            // Section 792
            TeXError::ExtraAlignmentTab => {
                print_err!("Extra alignment tab has been changed to ");
                self.print_esc("cr");
                help_lines!(
                    "You have given more \\span or & marks than there were",
                    "in the preamble to the \\halign or \\valign now in progress.",
                    "So I'll assume that you meant to type \\cr instead."
                )
            },

            // Section 826
            TeXError::InfiniteGlueShrinkageInParagraph => {
                print_err!("Infinite glue shrinkage found in a paragraph");
                help_lines!(
                    "The paragraph just ended includes some glue that has",
                    "infinite shrinkability, e.g., `\\hskip 0pt minus 1fil'.",
                    "Such glue doesn't belong there---it allows a paragraph",
                    "of any length to fit on one line. But it's safe to proceed,",
                    "since the offensive shrinkability has been made finite."
                )
            },

            // Section 936
            TeXError::ImproperHyphenation => {
                print_err!("Improper ");
                self.print_esc("hyphenation will be flushed");
                help_lines!(
                    "Hyphenation exceptions must contain only letters",
                    "and hyphens."
                )
            },

            // Section 937
            TeXError::NotALetter => {
                print_err!("Not a letter.");
                help_lines!("Letters in \\hyphenation words must have \\lccode>0.")
            },

            // Section 960
            TeXError::TooLateForPatterns => {
                print_err!("Too late for ");
                self.print_esc("patterns");
                help_lines!("All patterns must be given before typesetting begins.")
            },

            // Section 961
            TeXError::BadPatterns => {
                print_err!("Bad ");
                self.print_esc("patterns");
                help_lines!("(See Appendix H.)")
            },

            // Section 962
            TeXError::Nonletter => {
                print_err!("Nonletter");
                help_lines!("(See Appendix H.)")
            },

            // Section 963
            TeXError::DuplicatePattern => {
                print_err!("Duplicate pattern");
                help_lines!("(See Appendix H.)")
            },

            // Section 976
            TeXError::InfiniteGlueShrinkageInBoxBeingSplit => {
                print_err!("Infinite glue shrinkage found in box being split");
                help_lines!(
                    "The box you are \\vsplitting contains some infinitely",
                    "shrinkable glue, e.g., `\\vss' or `\\vskip 0pt minus 1fil'.",
                    "Such glue doesn't belong there."
                )
            },

            // Section 978
            TeXError::VsplitNeedsAVbox => {
                print_err!("");
                self.print_esc("vsplit needs a ");
                self.print_esc("vbox");
                help_lines!(
                    "The box you are trying to split is an \\hbox.",
                    "I can't split such a box, so I'll leave it alone."
                )
            },

            // Section 993
            TeXError::InsertionCanOnlyBeAddedToVbox(_) => {
                print_err!("Insertions can only be added to a vbox");
                help_lines!(
                    "Tut tut: You're trying to \\insert into a",
                    "\\box register that now contains an \\hbox.",
                    "Proceed, and I'll discard its present contents."
                )
            },

            // Section 1004
            TeXError::InfiniteGlueShrinkageOnCurrentPage => {
                print_err!("Infinite glue shrinkage found on current page");
                help_lines!(
                    "The page about to be output contains some infinitely",
                    "shrinkable glue, e.g., `\\vss' or `\\vskip 0pt minus 1fil'.",
                    "Such glue doesn't belong there; but you can safely proceed,",
                    "since the offensive shrinkability has been made finite."
                )
            },

            // Section 1009
            TeXError::InfiniteGlueShrinkageInsertedFrom(n) => {
                print_err!("Infinite glue shrinkage inserted from ");
                self.print_esc("skip");
                self.print_int(n);
                help_lines!(
                    "The correction glue for page breaking with insertions",
                    "must have finite shrinkability. But you may proceed,",
                    "since the offensive shrinkability has been made finite."
                )
            },

            // Section 1015
            TeXError::Box255IsNotVoid => {
                print_err!("");
                self.print_esc("box255 is not void");
                help_lines!(
                    "You shouldn't use \\box255 except in \\output routines.",
                    "Proceed, and I'll discard its present contents."
                )
            },

            // Section 1024
            TeXError::OutputLoop => {
                print_err!("Output loop---");
                self.print_int(self.dead_cycles);
                self.print(" consecutive dead cycles");
                help_lines!(
                    "I've concluded that your \\output is awry; it never does a",
                    "\\shipout, so I'm shipping \\box255 out myself. Next time",
                    "increase \\maxdeadcycles if you want me to be more patient!"
                )
            },

            // Section 1027
            TeXError::UnbalancedOutputRoutine => {
                print_err!("Unbalanced output routine");
                help_lines!(
                    "Your sneaky output routine has problematic {'s and/or }'s.",
                    "I can't handle that very well; good luck."
                )
            },

            // Section 1028
            TeXError::OutputRoutineDidntUseAllOfBox255 => {
                print_err!("Output routine didn't use all of ");
                self.print_esc("box");
                self.print_int(255);
                help_lines!(
                    "Your \\output commands should empty \\box255,",
                    "e.g., by saying `\\shipout\\box255'.",
                    "Proceed; I'll discard its present contents."
                )
            },

            // Section 1050
            TeXError::ReportIllegalCase => {
                you_cant!();
                help_lines!(
                    "Sorry, but I'm not programmed to handle this case;",
                    "I'll just pretend that you didn't ask for it.",
                    "If you're in the wrong mode, you might be able to",
                    "return to the right one by typing `I}' or `I$' or `I\\par'."
                )
            },

            // Section 1047
            TeXError::MissingDollar => {
                print_err!("Missing $ inserted");
                help_lines!(
                    "I've inserted a begin-math/end-math symbol since I think",
                    "you left one out. Proceed, with fingers crossed."
                )
            },

            // Section 1064, 1065
            // Message is printed by off_save before calling error
            TeXError::MissingEndGroup => {
                help_lines!(
                    "I've inserted something that you may have forgotten.",
                    "(See the <inserted text> above.)",
                    "With luck, this will get me unwedged. But if you",
                    "really didn't forget anything, try typing `2' now; then",
                    "my insertion and my current dilemma will both disappear."
                )
            },

            TeXError::MissingMathRight => {
                print_err!("Missing ");
                self.print_esc("right");
                help_lines!("Groups not properly nested (did you forget a \"\\right.\"?")
            },

            TeXError::MissingRightBrace => {
                print_err!("Missing }");
                help_lines!("Groups not properly nested (did you forget a right brace?)")
            },

            // Section 1066
            TeXError::Extra => {
                print_err!("Extra ");
                self.print_cmd_chr(self.cur_cmd, self.cur_chr);
                help_lines!("Things are pretty mixed up, but I think the worst is over.")
            },

            // Section 1068
            TeXError::TooManyRightBraces => {
                print_err!("Too many }'s");
                help_lines!(
                    "You've closed more groups than you opened.",
                    "Such booboos are generally harmless, so keep going."
                )
            },

            // Section 1069
            TeXError::ExtraRightBraceOrForgotten => {
                print_err!("Extra }, or forgotten ");
                match self.cur_group {
                    SEMI_SIMPLE_GROUP => self.print_esc("endgroup"),
                    MATH_SHIFT_GROUP => self.print("$"),
                    MATH_LEFT_GROUP => self.print_esc("right"),
                    _ => (), // Cannot happen
                }
                help_lines!(
                    "I've deleted a group-closing symbol because it seems to be",
                    "spurious, as in `$x}$'. But perhaps the } is legitimate and",
                    "you forgot something else, as in `\\hbox{$x}'. In such cases",
                    "the way to recover is to insert both the forgotten and the",
                    "deleted material, e.g., by typing `I$}'."
                )
            },

            // Section 1078
            TeXError::LeadersNotFollowedByProperGlue => {
                print_err!("Leaders not followed by proper glue");
                help_lines!(
                    "You should say `\\leaders <box or rule><hskip or vskip>'.",
                    "I found the <box or rule>, but there's no suitable",
                    "<hskip or vskip>, so I'm ignoring these leaders."
                )
            },

            // Section 1080
            TeXError::CantUseIn => {
                you_cant!();
                help_lines!("Sorry")
            },

            TeXError::CantUseIn2 => {
                you_cant!();
                help_lines!("Sorry...I usually can't take things from the current page.")
            },

            TeXError::LastBoxVoidMath => {
                you_cant!();
                help_lines!("Sorry; this \\lastbox will be void.")
            },

            TeXError::LastBoxVoidVmode => {
                you_cant!();
                help_lines!(
                    "Sorry...I usually can't take things from the current page.",
                    "This \\lastbox will therefore be void."
                )
            },

            // Section 1082
            TeXError::MissingTo => {
                print_err!("Missing `to' inserted");
                help_lines!(
                    "I'm working on `\\vsplit<box number> to <dimen>';",
                    "will look for the <dimen> next."
                )
            },

            // Section 1084
            TeXError::BoxWasSupposedToBeHere => {
                print_err!("A <box> was supposed to be here");
                help_lines!(
                    "I was expecting to see \\hbox or \\vbox or \\copy or \\box or",
                    "something like that. So you might find something missing in",
                    "your output. But keep trying; you can fix this later."
                )
            },

            // Section 1084
            TeXError::CantUseHrule => {
                print_err!("You can't use `");
                self.print_esc("hrule");
                self.print("' here except with leaders");
                help_lines!(
                    "To put a horizontal rule in an hbox or an alignment,",
                    "you should use \\leaders or \\hrulefill (see The TeXbook)."
                )
            },

            // Section 1099
            TeXError::CantInsert255 => {
                print_err!("You can't ");
                self.print_esc("insert");
                self.print_int(255);
                help_lines!("I'm changing to \\insert0; box 255 is special.")
            },

            // Section 1106
            TeXError::CantTakeThings => {
                you_cant!();
                if self.cur_chr == KERN_NODE as HalfWord {
                    help_lines!(
                        "Sorry...I usually can't take things from the current page.",
                        "Try `I\\kern-\\lastkern' instead."
                    )
                }
                else if self.cur_chr != GLUE_NODE as HalfWord {
                    help_lines!(
                        "Sorry...I usually can't take things from the current page.",
                        "Perhaps you can make the output routine do it."
                    )
                }
                else {
                    help_lines!(
                        "Sorry...I usually can't take things from the current page.",
                        "Try `I\\vskip-\\lastskip' instead."
                    )
                }
            },

            // Section 1110
            TeXError::IncompatibleListCantBeUnboxed => {
                print_err!("Incompatible list can't be unboxed");
                help_lines!(
                    "Sorry, Pandora. (You sneaky devil.)",
                    "I refuse to unbox an \\hbox in vertical mode or vice versa.",
                    "And I can't open any boxes in math mode."
                )
            },

            // Section 1120
            TeXError::IllegalMathDisc => {
                print_err!("Illegal math ");
                self.print_esc("discretionary");
                help_lines!(
                    "Sorry: The third part of a discretionary break must be",
                    "empty, in math formulas. I had to delete your third part."
                )
            },

            TeXError::DiscListTooLong => {
                print_err!("Discretionary list is too long");
                help_lines!(
                    "Wow---I never thought anybody would tweak me here.",
                    "You can't seriously need such a huge discretionary list?"
                )
            },

            // Section 1121
            TeXError::ImproperDiscList => {
                print_err!("Improper discretionary list");
                help_lines!("Discretionary lists must contain only boxes and kerns.")
            },

            // Section 1127
            // Message is printed by align_error before calling ins_error
            TeXError::MissingLeftBrace3 => {
                help_lines!(
                    "I've put in what seems to be necessary to fix",
                    "the current column of the current alignment.",
                    "Try to go on, since this might almost work."
                )
            },
            TeXError::MissingRightBrace2 => {
                print_err!("Missing }");
                help_lines!(
                    "Something needs to be done to fix the current column",
                    "of the current alignment."
                )
            },

            // Section 1128
            TeXError::MisplacedTabMark => {
                print_err!("Misplaced ");
                self.print_cmd_chr(self.cur_cmd, self.cur_chr);
                if self.cur_tok == TAB_TOKEN + b'&' as HalfWord {
                    help_lines!(
                        "I can't figure out why you would want to use a tab mark",
                        "here. If you just want an ampersand, the remedy is",
                        "simple: Just type `I\\&' now. But if some right brace",
                        "up above has ended a previous alignment prematurely,",
                        "you're probably due for more error messages, and you",
                        "might try typing `S' now just to see what is salvageable."
                    )
                }
                else {
                    help_lines!(
                        "I can't figure out why you would want to use a tab mark",
                        "or \\cr or \\span just now. If something like a right brace",
                        "up above has ended a previous alignment prematurely,",
                        "you're probably due for more error messages, and you",
                        "might try typing `S' now just to see what is salvageable."
                    )
                }
            },

            // Section 1129
            TeXError::MisplacedNoalign => {
                print_err!("Misplaced ");
                self.print_esc("noalign");
                help_lines!(
                    "I expect to see \\noalign only after the \\cr of",
                    "an alignment. Proceed, and I'll ignore this case."
                )
            },

            TeXError::MisplacedOmit => {
                print_err!("Misplaced ");
                self.print_esc("omit");
                help_lines!(
                    "I expect to see \\omit only after tab marks or the \\cr of",
                    "an alignment. Proceed, and I'll ignore this case."
                )
            },

            // Section 1132
            TeXError::MissingCr => {
                print_err!("Missing ");
                self.print_esc("cr");
                self.print(" inserted");
                help_lines!("I'm guessing that you meant to end an alignment here.")
            },

            // Section 1135
            TeXError::ExtraEndcsname => {
                print_err!("Extra ");
                self.print_esc("endcsname");
                help_lines!("I'm ignoring this, since I wasn't doing a \\csname.")
            },

            // Section 1159
            TeXError::LimitControlsMustFollowMathOp => {
                print_err!("Limit controls must follow a math operator");
                help_lines!("I'm ignoring this misplaced \\limits or \\nolimits command.")
            },

            // Section 1161
            TeXError::MissingDelimiterLeftParen => {
                print_err!("Missing delimiter (. inserted)");
                help_lines!(
                    "I was expecting to see something like `(' or `\\{' or",
                    "`\\}' here. If you typed, e.g., `{' instead of `\\{', you",
                    "should probably delete the `{' by typing `1' now, so that",
                    "braces don't get unbalanced. Otherwise just proceed.",
                    "Acceptable delimiters are characters whose \\delcode is",
                    "nonnegative, or you can use `\\delimiter <delimiter code>'."
                )
            },

            // Section 1166
            TeXError::UseMathAccentInMathMode => {
                print_err!("Please use ");
                self.print_esc("mathaccent");
                self.print(" for accents in math mode");
                help_lines!(
                    "I'm changing \\accent to \\mathaccent here; wish me luck.",
                    "(Accents are not the same in formulas as they are in text.)"
                )
            },

            // Section 1177
            TeXError::DoubleSuperscript => {
                print_err!("Double superscript");
                help_lines!(
                    "You wrote something like x^1^2 which is confusing:",
                    "did you mean {x^1}^2 or x^{1^2} (or something else)?"
                )
            },

            TeXError::DoubleSubscript => {
                print_err!("Double subscript");
                help_lines!(
                    "You wrote something like x_1_2 which is confusing:",
                    "did you mean {x_1}_2 or x_{1_2} (or something else)?"
                )
            },

            // Section 1183
            TeXError::AmbiguousFraction => {
                print_err!("Ambiguous; you need another { and }");
                help_lines!(
                    "I'm ignoring this fraction specification, since I don't",
                    "know whether a construction like `x \\over y \\over z'",
                    "means `{x \\over y} \\over z' or `x \\over {y \\over z}'."
                )
            },

            // Section 1192
            TeXError::ExtraMathRight => {
                print_err!("Extra ");
                self.print_esc("right");
                help_lines!("I'm ignoring a \\right that had no matching \\left.")
            },

            // Section 1195
            TeXError::InsufficientSymbolFonts => {
                print_err!("Math formula deleted: Insufficient symbol fonts");
                help_lines!(
                    "Sorry, but I can't typeset math unless \\textfont 2",
                    "and \\scriptfont 2 and \\scriptscriptfont 2 have all",
                    "the \\fontdimen values needed in math symbol fonts."
                )
            },

            TeXError::InsufficientExtensionFonts => {
                print_err!("Math formula deleted: Insufficient extension fonts");
                help_lines!(
                    "Sorry, but I can't typeset math unless \\textfont 3",
                    "and \\scriptfont 3 and \\scriptscriptfont 3 have all",
                    "the \\fontdimen values needed in math extension fonts."
                )
            },

            // Section 1197
            TeXError::DisplayMathEndsWithDollars => {
                print_err!("Display math should end with $$");
                help_lines!(
                    "The `$' that I just saw supposedly matches a previous `$$'.",
                    "So I shall assume that you typed `$$' both times."
                )
            },

            // Section 1207
            TeXError::MissingDollarDollar => {
                print_err!("Missing $$ inserted");
                help_lines!(
                    "Displays can use special alignments (like \\eqalignno)",
                    "only if nothing but the alignment itself is between $$'s."
                )
            },

            // Section 1212
            TeXError::CantUsePrefix => {
                print_err!("You can't use a prefix with `");
                self.print_cmd_chr(self.cur_cmd, self.cur_chr);
                self.print("'");
                help_lines!(
                    "I'll pretend you didn't say \\long or \\outer or \\global."
                )
            },

            // Section 1213
            TeXError::CantUseLongOuter => {
                print_err!("You can't use `");
                self.print_esc("long");
                self.print("' or `");
                self.print_esc("outer");
                self.print("' with `");
                self.print_cmd_chr(self.cur_cmd, self.cur_chr);
                self.print("'");
                help_lines!("I'll pretend you didn't say \\long or \\outer here.")
            },

            // Section 1215
            TeXError::MissingControlSequence => {
                print_err!("Missing control sequence inserted");
                help_lines!(
                    "Please don't say `\\def cs{...}', say `\\def\\cs{...}'.",
                    "I've inserted an inaccessible control sequence so that your",
                    "definition will be completed without mixing me up too badly.",
                    "You can recover graciously from this error, if you're",
                    "careful; see exercise 27.2 in The TeXbook."
                )
            },

            // Section 1225
            TeXError::MissingTo2 => {
                print_err!("Missing `to'");
                help_lines!("You should have said `\\read<number> to \\cs'.")
            },

            // Section 1232
            TeXError::InvalidCode(n, p) => {
                print_err!("Invalid code (");
                self.print_int(self.cur_val);
                self.print(
                    if p < DEL_CODE_BASE {
                        "), should be in the range 0.."
                    }
                    else {
                        "), should be at most "
                    }
                );
                self.print_int(n);
                help_lines!("You used an illegal code value.")
            },

            // Section 1236
            TeXError::Arith => {
                print_err!("Arithmetic overflow");
                help_lines!(
                    "I can't carry out that multiplication or division,",
                    "since the result is out of range."
                )
            },

            // Section 1237
            TeXError::CantUseAfterCmd(q) => {
                print_err!("You can't use `");
                self.print_cmd_chr(self.cur_cmd, self.cur_chr);
                self.print("' after ");
                self.print_cmd_chr(q, 0);
                help_lines!("I'm forgetting what you said and not changing anything.")
            },

            // Section 1241
            TeXError::ImproperSetbox => {
                print_err!("Improper ");
                self.print_esc("setbox");
                help_lines!(
                    "Sorry, \\setbox is not allowed after \\halign in a display,",
                    "or between \\accent and an accented character."
                )
            },

            // Section 1243
            TeXError::BadSpaceFactor => {
                print_err!("Bad space factor");
                self.print(" (");
                self.print_int(self.cur_val);
                self.print(")");
                help_lines!("I allow only values in the range 1..32767 here.")
            },

            // Section 1244
            TeXError::BadPrevGraf => {
                print_err!("Bad ");
                self.print_esc("prevgraf (");
                self.print_int(self.cur_val);
                self.print(")");
                help_lines!("I allow only nonnegative values here.")
            },

            // Section 1252
            TeXError::PatternsOnlyIniTeX => {
                print_err!("Patterns can be loaded only by INITEX");
                help_lines!("")
            },

            // Section 1259
            TeXError::ImproperAt(s) => {
                print_err!("Improper `at' size (");
                self.print_scaled(s);
                self.print("pt), replaced by 10pt");
                help_lines!(
                    "I can only handle fonts at positive sizes that are",
                    "less than 2048pt, so I've changed what you said to 10pt."
                )
            },

            // Section 1283
            TeXError::ErrMessage(s) => {
                print_err!("");
                self.slow_print(s);
                if self.use_err_help {
                    // help text will be handled by give_err_help in error()
                    vec![]
                }
                else if self.long_help_seen {
                    help_lines!("(That was another \\errmessage.)")
                }
                else {
                    if self.interaction < ERROR_STOP_MODE {
                        self.long_help_seen = true;
                    }
                    help_lines!(
                        "This error message was generated by an \\errmessage",
                        "command, so I can't give any explicit help.",
                        "Pretend that you're Hercule Poirot: Examine all clues,",
                        "and deduce the truth by order and method."
                    )
                }
            },

            // Section 1293: show_whatever common_ending
            // No print_err! — the message ("> ..." or "! OK") was already printed.
            TeXError::ShowWhatever => {
                if self.interaction < ERROR_STOP_MODE {
                    // help0: no help text. error_count was pre-decremented
                    // by the caller so the net change after error() increments is 0.
                    vec![]
                }
                else if tracing_online() > 0 {
                    help_lines!(
                        "This isn't an error message; I'm just \\showing something.",
                        "Type `I\\show...' to show more (e.g., \\show\\cs,",
                        "\\showthe\\count10, \\showbox255, \\showlists)."
                    )
                }
                else {
                    help_lines!(
                        "This isn't an error message; I'm just \\showing something.",
                        "Type `I\\show...' to show more (e.g., \\show\\cs,",
                        "\\showthe\\count10, \\showbox255, \\showlists).",
                        "And type `I\\tracingonline=1\\show...' to show boxes and",
                        "lists on your terminal as well as in the transcript file."
                    )
                }
            },

            // Section 1304
            TeXError::CantDumpInGroup => {
                print_err!("You can't dump inside a group");
                help_lines!("'{...\\dump}' is a no-no.")
            },

            // Section 1372
            TeXError::UnbalancedWriteCmd => {
                print_err!("Unbalanced write command");
                help_lines!(
                    "On this page there's a \\write with fewer real {'s than }'s.",
                    "I can't handle that very well; good luck."
                )
            }

            // Format
            TeXError::CantFindFormat => {
                print_err!("Sorry, I can't find that format");
                help_lines!(
                    "Either you got the name wrong (e.g., 'plaine.fmt'",
                    "instead of 'plain.fmt') or the format is not present",
                    "in the format directory or the current one."
                )
            }
        };

        // Section 82: Complete the error reporting (like TeX82's error procedure)
        if self.history < ERROR_MESSAGE_ISSUED {
            self.history = ERROR_MESSAGE_ISSUED;
        }

        self.print_char(b'.');

        // Show context
        self.show_context();

        // Section 83: Get user's advice if in error_stop_mode
        if self.interaction == ERROR_STOP_MODE {
            // Interactive error handling
            loop {
                // Clear for error prompt
                self.print_ln();
                if self.interaction != ERROR_STOP_MODE {
                    break;
                }
                // prompt_input("? ")
                self.print("? ");
                update_terminal!();
                let input = term_input_string()?;
                let trimmed = input.trim();
                if trimmed.is_empty() {
                    break;
                }
                let c = trimmed.as_bytes()[0];
                let c = if c >= b'a' && c <= b'z' { c - b'a' + b'A' } else { c };
                match c {
                    b'Q' | b'R' | b'S' => {
                        // Section 86: Change interaction level
                        self.error_count = 0;
                        self.interaction = BATCH_MODE + (c - b'Q') as Integer;
                        self.print("OK, entering ");
                        match c {
                            b'Q' => {
                                self.print_esc("batchmode");
                                self.selector -= 1;
                            },
                            b'R' => self.print_esc("nonstopmode"),
                            b'S' => self.print_esc("scrollmode"),
                            _ => (),
                        }
                        self.print("...");
                        self.print_ln();
                        update_terminal!();
                        break;
                    },
                    b'H' => {
                        // Section 89: Print help information
                        if help_message.is_empty() {
                            self.print_nl("Sorry, I don't know how to help in this situation.");
                            self.print_nl("Maybe you should try asking a human?");
                        } else {
                            for help_line in &help_message {
                                self.print_nl(*help_line);
                            }
                        }
                        self.print_nl("Sorry, I already gave what help I could...");
                        self.print_nl("Maybe you should try asking a human?");
                        self.print_nl("An error might have occurred before I noticed any problems.");
                        self.print_nl("``If all else fails, read the instructions.''");
                        continue;
                    },
                    b'I' => {
                        // Section 87: Introduce new material from the terminal
                        self.begin_file_reading()?;
                        let bytes = trimmed[1..].trim_start().as_bytes();
                        if bytes.is_empty() {
                            self.print("insert>");
                            update_terminal!();
                            let insert_input = term_input_string()?;
                            let insert_bytes = insert_input.trim().as_bytes();
                            let len = insert_bytes.len();
                            self.buffer[self.first as usize..self.first as usize + len]
                                .copy_from_slice(insert_bytes);
                            self.last = self.first + len as Integer;
                            *self.loc_mut() = self.first;
                        } else {
                            let len = bytes.len();
                            self.buffer[self.first as usize] = b' ';
                            self.buffer[self.first as usize + 1..self.first as usize + 1 + len]
                                .copy_from_slice(bytes);
                            self.last = self.first + 1 + len as Integer;
                            *self.loc_mut() = self.first + 1;
                            self.buffer[self.first as usize] = b' ';
                        }
                        self.first = self.last;
                        *self.limit_mut() = self.last - 1;
                        break;
                    },
                    b'X' => {
                        self.interaction = SCROLL_MODE;
                        return Err(TeXError::Fatal("interrupted by the user"));
                    },
                    b'0'..=b'9' if self.deletions_allowed => {
                        // Section 88: Delete tokens
                        let s1 = self.cur_tok;
                        let s2 = self.cur_cmd;
                        let s3 = self.cur_chr;
                        let s4 = self.align_state;
                        self.align_state = 1_000_000;
                        self.ok_to_interrupt = false;
                        let mut count = (c - b'0') as Integer;
                        // Check for second digit
                        if trimmed.len() > 1 {
                            let c2 = trimmed.as_bytes()[1];
                            if c2 >= b'0' && c2 <= b'9' {
                                count = count * 10 + (c2 - b'0') as Integer;
                            }
                        }
                        while count > 0 {
                            self.get_token()?;
                            count -= 1;
                        }
                        self.cur_tok = s1;
                        self.cur_cmd = s2;
                        self.cur_chr = s3;
                        self.align_state = s4;
                        self.ok_to_interrupt = true;
                        self.print_nl("I have just deleted some text, as you asked.");
                        self.print_nl("You can now delete more, or insert, or whatever.");
                        self.show_context();
                        continue;
                    },

                    #[cfg(feature = "debug")]
                    b'D' => {
                        self.debug_help()?;
                        continue;
                    },

                    _ => {
                        // Section 90: Print the menu of available options
                        self.print("Type <return> to proceed, S to scroll future error messages,");
                        self.print_nl("R to run without stopping, Q to run quietly,");
                        self.print_nl("I to insert something, ");
                        if self.deletions_allowed {
                            self.print_nl("1 or ... or 9 to ignore the next 1 to 9 tokens of input,");
                        }
                        self.print_nl("H for help, X to quit.");
                        continue;
                    },
                }
            }
        }

        // Section 85: Increment error count
        self.error_count += 1;
        if self.error_count == 100 {
            self.print_nl("(That makes 100 errors; please try again.)");
            self.history = FATAL_ERROR_STOP;
            return Err(TeXError::Fatal("too many errors"));
        }

        // Section 91: Put help message on the transcript file
        if self.interaction > BATCH_MODE {
            self.selector -= 1; // avoid terminal output
        }
        if self.use_err_help {
            self.print_ln();
            self.token_show(equiv(ERR_HELP_LOC));
            self.use_err_help = false;
        } else {
            for help_line in &help_message {
                self.print_nl(*help_line);
            }
        }
        self.print_ln();
        if self.interaction > BATCH_MODE {
            self.selector += 1; // re-enable terminal output
        }
        self.print_ln();

        // Supplementary messages
        match texerror {
            TeXError::InsertionCanOnlyBeAddedToVbox(n) => self.box_error(n)?,
            TeXError::Box255IsNotVoid => self.box_error(255)?,
            TeXError::OutputRoutineDidntUseAllOfBox255 => self.box_error(255)?,
            _ => (),
        }

        Ok(())
    }
}
