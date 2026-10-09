signature HTML_TOKENS =
sig
type ('a,'b) token
type svalue
val ENTITY_REF: (string) *  'a * 'a -> (svalue,'a) token
val CHAR_REF: (string) *  'a * 'a -> (svalue,'a) token
val PCDATA: (string) *  'a * 'a -> (svalue,'a) token
val END_VAR:  'a * 'a -> (svalue,'a) token
val START_VAR:  'a * 'a -> (svalue,'a) token
val END_UL:  'a * 'a -> (svalue,'a) token
val START_UL: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_U:  'a * 'a -> (svalue,'a) token
val START_U:  'a * 'a -> (svalue,'a) token
val END_TT:  'a * 'a -> (svalue,'a) token
val START_TT:  'a * 'a -> (svalue,'a) token
val END_TR:  'a * 'a -> (svalue,'a) token
val START_TR: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_TITLE:  'a * 'a -> (svalue,'a) token
val START_TITLE:  'a * 'a -> (svalue,'a) token
val END_TH:  'a * 'a -> (svalue,'a) token
val START_TH: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_TEXTAREA:  'a * 'a -> (svalue,'a) token
val START_TEXTAREA: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_TD:  'a * 'a -> (svalue,'a) token
val START_TD: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_TABLE:  'a * 'a -> (svalue,'a) token
val START_TABLE: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_SUP:  'a * 'a -> (svalue,'a) token
val START_SUP:  'a * 'a -> (svalue,'a) token
val END_SUB:  'a * 'a -> (svalue,'a) token
val START_SUB:  'a * 'a -> (svalue,'a) token
val END_STYLE:  'a * 'a -> (svalue,'a) token
val START_STYLE:  'a * 'a -> (svalue,'a) token
val END_STRONG:  'a * 'a -> (svalue,'a) token
val START_STRONG:  'a * 'a -> (svalue,'a) token
val END_STRIKE:  'a * 'a -> (svalue,'a) token
val START_STRIKE:  'a * 'a -> (svalue,'a) token
val END_SMALL:  'a * 'a -> (svalue,'a) token
val START_SMALL:  'a * 'a -> (svalue,'a) token
val END_SELECT:  'a * 'a -> (svalue,'a) token
val START_SELECT: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_SCRIPT:  'a * 'a -> (svalue,'a) token
val START_SCRIPT:  'a * 'a -> (svalue,'a) token
val END_SAMP:  'a * 'a -> (svalue,'a) token
val START_SAMP:  'a * 'a -> (svalue,'a) token
val END_PRE:  'a * 'a -> (svalue,'a) token
val START_PRE: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val TAG_PARAM: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_P:  'a * 'a -> (svalue,'a) token
val START_P: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_OPTION:  'a * 'a -> (svalue,'a) token
val START_OPTION: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_OL:  'a * 'a -> (svalue,'a) token
val START_OL: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val TAG_META: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_MENU:  'a * 'a -> (svalue,'a) token
val START_MENU: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_MAP:  'a * 'a -> (svalue,'a) token
val START_MAP: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val TAG_LINK: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_LI:  'a * 'a -> (svalue,'a) token
val START_LI: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_KBD:  'a * 'a -> (svalue,'a) token
val START_KBD:  'a * 'a -> (svalue,'a) token
val TAG_ISINDEX: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val TAG_INPUT: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val TAG_IMG: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_I:  'a * 'a -> (svalue,'a) token
val START_I:  'a * 'a -> (svalue,'a) token
val END_HTML:  'a * 'a -> (svalue,'a) token
val START_HTML:  'a * 'a -> (svalue,'a) token
val TAG_HR: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_HEAD:  'a * 'a -> (svalue,'a) token
val START_HEAD:  'a * 'a -> (svalue,'a) token
val END_H6:  'a * 'a -> (svalue,'a) token
val START_H6: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_H5:  'a * 'a -> (svalue,'a) token
val START_H5: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_H4:  'a * 'a -> (svalue,'a) token
val START_H4: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_H3:  'a * 'a -> (svalue,'a) token
val START_H3: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_H2:  'a * 'a -> (svalue,'a) token
val START_H2: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_H1:  'a * 'a -> (svalue,'a) token
val START_H1: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_FORM:  'a * 'a -> (svalue,'a) token
val START_FORM: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_BASEFONT:  'a * 'a -> (svalue,'a) token
val START_BASEFONT: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_FONT:  'a * 'a -> (svalue,'a) token
val START_FONT: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_EM:  'a * 'a -> (svalue,'a) token
val START_EM:  'a * 'a -> (svalue,'a) token
val END_DT:  'a * 'a -> (svalue,'a) token
val START_DT:  'a * 'a -> (svalue,'a) token
val END_DL:  'a * 'a -> (svalue,'a) token
val START_DL: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_DIV:  'a * 'a -> (svalue,'a) token
val START_DIV: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_DIR:  'a * 'a -> (svalue,'a) token
val START_DIR: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_DFN:  'a * 'a -> (svalue,'a) token
val START_DFN:  'a * 'a -> (svalue,'a) token
val END_DD:  'a * 'a -> (svalue,'a) token
val START_DD:  'a * 'a -> (svalue,'a) token
val END_CODE:  'a * 'a -> (svalue,'a) token
val START_CODE:  'a * 'a -> (svalue,'a) token
val END_CITE:  'a * 'a -> (svalue,'a) token
val START_CITE:  'a * 'a -> (svalue,'a) token
val END_CENTER:  'a * 'a -> (svalue,'a) token
val START_CENTER:  'a * 'a -> (svalue,'a) token
val END_CAPTION:  'a * 'a -> (svalue,'a) token
val START_CAPTION: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val TAG_BR: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_BODY:  'a * 'a -> (svalue,'a) token
val START_BODY: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_BLOCKQUOTE:  'a * 'a -> (svalue,'a) token
val START_BLOCKQUOTE:  'a * 'a -> (svalue,'a) token
val END_BIG:  'a * 'a -> (svalue,'a) token
val START_BIG:  'a * 'a -> (svalue,'a) token
val TAG_BASE: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_B:  'a * 'a -> (svalue,'a) token
val START_B:  'a * 'a -> (svalue,'a) token
val TAG_AREA: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_APPLET:  'a * 'a -> (svalue,'a) token
val START_APPLET: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val END_ADDRESS:  'a * 'a -> (svalue,'a) token
val START_ADDRESS:  'a * 'a -> (svalue,'a) token
val END_A:  'a * 'a -> (svalue,'a) token
val START_A: (HTMLAttrVals.attrs) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature HTML_LRVALS=
sig
structure Tokens : HTML_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
