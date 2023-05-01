[newpp]/smlnj/smlnj-lib/Dev/PrettyPrint/src90

README for PrettyPrint Version 9.0
[DBM: 2023.3.3]

This version adds test styling.

1. Styles are defined in structure Style [style.sml]. There are [tenatively] two kinds of style supported:

   * colored text [RED, BLUE, GREEN, ...]
   * type face variants: [BOLD, ITALICS, (REGULAR), ...] (the default is REGULAR)

2. The Format.format type has a new constructor:

   datatype format = ... | STYLE of (style, format)

   This represents applying a style to the specified format.
   Any format can be styled, but the styles apply only

3. Styles are assumed not to modify character widths (and hence, nor strengh sizes). Characters are
   still assumed to have fixed and uniform width.  So sizes can still be measured in characters.
   Thus for the measure function

     measure (STYLE (s, format)) = measure format

   Note: variable width characters can be addressed when they are generally supported in SML/NJ.

4. The rendering function(s) take a "style context", which is initially a stack (list) of styles.

     type style_context = style list

   This style context is passed as a separate parameter down through the traversal of formats when
   they are rendered.  When we get to a TEXT format, the rendering is

       render (TEXT s, styles, ...) = output (s, styles)

   That is, the "output" function is responsible for interpreting the style context and applying it
   when printing the string s.  This interpretation could either just use the latest, innermost style,
   which is the head of the style context list, or it can "cascade" the nested styles somehow.
   Some styles might be compatible, e.g.

       COLOR BLUE :: FACE BOLD :: ...

       FACE BOLD :: FACE ITALIC :: ...  -- presumably produces bold-italic text

   and some might be incompabile, e.g.

       COLOR BLUE :: COLOR GREEN :: ... (assuming we are not "blending" colors)

   It would also be possible to define a special "style context" type that was not just a stack
   of styles, and when a new style is introduced by the STYLE constructor, that new style could
   be "merged into" the prevailing (inherited) style context to define the style context for the
   styled format.

