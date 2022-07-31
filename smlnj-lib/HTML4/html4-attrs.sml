(* html4-attrs.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Helper functions for creating attribute-value pairs to include in
 * HTML tags.  We omit the deprecated attributes (commented out below),
 * but include both the strict, loose, and frameset attributes.
 *
 * The source of the attribute list comes from
 *
 *	http://www.w3.org/TR/html4/index/attributes.html
 *)

structure HTML4Attrs =
  struct

    local
      val a_abbr = Atom.atom "abbr"
      val a_accept_charset = Atom.atom "accept-charset"
      val a_accept = Atom.atom "accept"
      val a_action = Atom.atom "action"
      val a_align = Atom.atom "align"
      val a_alink = Atom.atom "alink"
      val a_alt = Atom.atom "alt"
      val a_archive = Atom.atom "archive"
      val a_axis = Atom.atom "axis"
      val a_background = Atom.atom "background"
      val a_bgcolor = Atom.atom "bgcolor"
      val a_border = Atom.atom "border"
      val a_cellpadding = Atom.atom "cellpadding"
      val a_cellspacing = Atom.atom "cellspacing"
      val a_char = Atom.atom "char"
      val a_charoff = Atom.atom "charoff"
      val a_charset = Atom.atom "charset"
      val a_checked = Atom.atom "checked"
      val a_cite = Atom.atom "cite"
      val a_class = Atom.atom "class"
      val a_classid = Atom.atom "classid"
      val a_clear = Atom.atom "clear"
      val a_code = Atom.atom "code"
      val a_codebase = Atom.atom "codebase"
      val a_codetype = Atom.atom "codetype"
      val a_color = Atom.atom "color"
      val a_cols = Atom.atom "cols"
      val a_colspan = Atom.atom "colspan"
      val a_compact = Atom.atom "compact"
      val a_content = Atom.atom "content"
      val a_coords = Atom.atom "coords"
      val a_data = Atom.atom "data"
      val a_datetime = Atom.atom "datetime"
      val a_declare = Atom.atom "declare"
      val a_defer = Atom.atom "defer"
      val a_dir = Atom.atom "dir"
      val a_disabled = Atom.atom "disabled"
      val a_enctype = Atom.atom "enctype"
      val a_face = Atom.atom "face"
      val a_for = Atom.atom "for"
      val a_frame = Atom.atom "frame"
      val a_frameborder = Atom.atom "frameborder"
      val a_headers = Atom.atom "headers"
      val a_height = Atom.atom "height"
      val a_href = Atom.atom "href"
      val a_hreflang = Atom.atom "hreflang"
      val a_hspace = Atom.atom "hspace"
      val a_http_equiv = Atom.atom "http-equiv"
      val a_id = Atom.atom "id"
      val a_ismap = Atom.atom "ismap"
      val a_label = Atom.atom "label"
      val a_lang = Atom.atom "lang"
      val a_language = Atom.atom "language"
      val a_link = Atom.atom "link"
      val a_longdesc = Atom.atom "longdesc"
      val a_marginheight = Atom.atom "marginheight"
      val a_marginwidth = Atom.atom "marginwidth"
      val a_maxlength = Atom.atom "maxlength"
      val a_media = Atom.atom "media"
      val a_method = Atom.atom "method"
      val a_multiple = Atom.atom "multiple"
      val a_name = Atom.atom "name"
      val a_nohref = Atom.atom "nohref"
      val a_noresize = Atom.atom "noresize"
      val a_noshade = Atom.atom "noshade"
      val a_nowrap = Atom.atom "nowrap"
      val a_object = Atom.atom "object"
      val a_onblur = Atom.atom "onblur"
      val a_onchange = Atom.atom "onchange"
      val a_onclick = Atom.atom "onclick"
      val a_ondblclick = Atom.atom "ondblclick"
      val a_onfocus = Atom.atom "onfocus"
      val a_onkeydown = Atom.atom "onkeydown"
      val a_onkeypress = Atom.atom "onkeypress"
      val a_onkeyup = Atom.atom "onkeyup"
      val a_onload = Atom.atom "onload"
      val a_onmousedown = Atom.atom "onmousedown"
      val a_onmousemove = Atom.atom "onmousemove"
      val a_onmouseout = Atom.atom "onmouseout"
      val a_onmouseover = Atom.atom "onmouseover"
      val a_onmouseup = Atom.atom "onmouseup"
      val a_onreset = Atom.atom "onreset"
      val a_onselect = Atom.atom "onselect"
      val a_onsubmit = Atom.atom "onsubmit"
      val a_onunload = Atom.atom "onunload"
      val a_profile = Atom.atom "profile"
      val a_prompt = Atom.atom "prompt"
      val a_readonly = Atom.atom "readonly"
      val a_rel = Atom.atom "rel"
      val a_rev = Atom.atom "rev"
      val a_rows = Atom.atom "rows"
      val a_rowspan = Atom.atom "rowspan"
      val a_rules = Atom.atom "rules"
      val a_scheme = Atom.atom "scheme"
      val a_scope = Atom.atom "scope"
      val a_scrolling = Atom.atom "scrolling"
      val a_selected = Atom.atom "selected"
      val a_shape = Atom.atom "shape"
      val a_size = Atom.atom "size"
      val a_span = Atom.atom "span"
      val a_src = Atom.atom "src"
      val a_standby = Atom.atom "standby"
      val a_start = Atom.atom "start"
      val a_style = Atom.atom "style"
      val a_summary = Atom.atom "summary"
      val a_tabindex = Atom.atom "tabindex"
      val a_target = Atom.atom "target"
      val a_text = Atom.atom "text"
      val a_title = Atom.atom "title"
      val a_type = Atom.atom "type"
      val a_usemap = Atom.atom "usemap"
      val a_valign = Atom.atom "valign"
      val a_value = Atom.atom "value"
      val a_valuetype = Atom.atom "valuetype"
      val a_version = Atom.atom "version"
      val a_vlink = Atom.atom "vlink"
      val a_vspace = Atom.atom "vspace"
      val a_width = Atom.atom "width"
    in
    fun abbr (v : string) = (a_abbr, SOME v)
    fun accept_charset (v : string) = (a_accept_charset, SOME v)
    fun accept (v : string) = (a_accept, SOME v)
    fun action (v : string) = (a_action, SOME v)
    fun align (v : string) = (a_align, SOME v)
    fun alink (v : string) = (a_alink, SOME v)
    fun alt (v : string) = (a_alt, SOME v)
    fun archive (v : string) = (a_archive, SOME v)
    fun axis (v : string) = (a_axis, SOME v)
(* DEPRECATED
    fun background (v : string) = (a_background, SOME v) (* deprecated *)
    fun bgcolor (v : string) = (a_bgcolor, SOME v) (* deprecated *)
*)
    fun border (v : string) = (a_border, SOME v)
    fun cellpadding (v : string) = (a_cellpadding, SOME v)
    fun cellspacing (v : string) = (a_cellspacing, SOME v)
    fun char (v : string) = (a_char, SOME v)
    fun charoff (v : string) = (a_charoff, SOME v)
    fun charset (v : string) = (a_charset, SOME v)
    fun checked (v : string) = (a_checked, SOME v)
    fun cite (v : string) = (a_cite, SOME v)
    fun class (v : string) = (a_class, SOME v)
    fun classid (v : string) = (a_classid, SOME v)
    fun clear (v : string) = (a_clear, SOME v)
    fun code (v : string) = (a_code, SOME v)
    fun codebase (v : string) = (a_codebase, SOME v)
    fun codetype (v : string) = (a_codetype, SOME v)
    fun color (v : string) = (a_color, SOME v)
    fun cols (v : string) = (a_cols, SOME v)
    fun colspan (v : string) = (a_colspan, SOME v)
(* DEPRECATED
    val compact = (a_compact, NONE) (* deprecated *)
*)
    fun content (v : string) = (a_content, SOME v)
    fun coords (v : string) = (a_coords, SOME v)
    fun data (v : string) = (a_data, SOME v)
    fun datetime (v : string) = (a_datetime, SOME v)
    val declare = (a_declare, NONE)
    val defer = (a_defer, NONE)
    fun dir (v : string) = (a_dir, SOME v)
    val disabled = (a_disabled, NONE)
    fun enctype (v : string) = (a_enctype, SOME v)
(* DEPRECATED
    fun face (v : string) = (a_face, SOME v) (* deprecated *)
*)
    fun for (v : string) = (a_for, SOME v)
    fun frame (v : string) = (a_frame, SOME v)
    fun frameborder (v : string) = (a_frameborder, SOME v)
    fun headers (v : string) = (a_headers, SOME v)
    fun height (v : string) = (a_height, SOME v)
    fun href (v : string) = (a_href, SOME v)
    fun hreflang (v : string) = (a_hreflang, SOME v)
    fun hspace (v : string) = (a_hspace, SOME v)
    fun http_equiv (v : string) = (a_http_equiv, SOME v)
    fun id (v : string) = (a_id, SOME v)
    fun ismap (v : string) = (a_ismap, SOME v)
    fun label (v : string) = (a_label, SOME v)
    fun lang (v : string) = (a_lang, SOME v)
(* DEPRECATED
    fun language (v : string) = (a_language, SOME v) (* deprecated *)
    fun link (v : string) = (a_link, SOME v) (* deprecated *)
*)
    fun longdesc (v : string) = (a_longdesc, SOME v)
    fun marginheight (v : string) = (a_marginheight, SOME v)
    fun marginwidth (v : string) = (a_marginwidth, SOME v)
    fun maxlength (v : string) = (a_maxlength, SOME v)
    fun media (v : string) = (a_media, SOME v)
    fun method (v : string) = (a_method, SOME v)
    fun multiple (v : string) = (a_multiple, SOME v)
    fun name (v : string) = (a_name, SOME v)
    val nohref = (a_nohref, NONE)
    val noresize = (a_noresize, NONE)
(* DEPRECATED
    val noshade = (a_noshade, NONE) (* deprecated *)
    val nowrap = (a_nowrap, NONE) (* deprecated *)
    fun object (v : string) = (a_object, SOME v) (* deprecated *)
*)
    fun onblur (v : string) = (a_onblur, SOME v)
    fun onchange (v : string) = (a_onchange, SOME v)
    fun onclick (v : string) = (a_onclick, SOME v)
    fun ondblclick (v : string) = (a_ondblclick, SOME v)
    fun onfocus (v : string) = (a_onfocus, SOME v)
    fun onkeydown (v : string) = (a_onkeydown, SOME v)
    fun onkeypress (v : string) = (a_onkeypress, SOME v)
    fun onkeyup (v : string) = (a_onkeyup, SOME v)
    fun onload (v : string) = (a_onload, SOME v)
    fun onmousedown (v : string) = (a_onmousedown, SOME v)
    fun onmousemove (v : string) = (a_onmousemove, SOME v)
    fun onmouseout (v : string) = (a_onmouseout, SOME v)
    fun onmouseover (v : string) = (a_onmouseover, SOME v)
    fun onmouseup (v : string) = (a_onmouseup, SOME v)
    fun onreset (v : string) = (a_onreset, SOME v)
    fun onselect (v : string) = (a_onselect, SOME v)
    fun onsubmit (v : string) = (a_onsubmit, SOME v)
    fun onunload (v : string) = (a_onunload, SOME v)
    fun profile (v : string) = (a_profile, SOME v)
(* DEPRECATED
    fun prompt (v : string) = (a_prompt, SOME v) (* deprecated *)
*)
    val readonly = (a_readonly, NONE)
    fun rel (v : string) = (a_rel, SOME v)
    fun rev (v : string) = (a_rev, SOME v)
    fun rows (v : string) = (a_rows, SOME v)
    fun rowspan (v : string) = (a_rowspan, SOME v)
    fun rules (v : string) = (a_rules, SOME v)
    fun scheme (v : string) = (a_scheme, SOME v)
    fun scope (v : string) = (a_scope, SOME v)
    fun scrolling (v : string) = (a_scrolling, SOME v)
    val selected = (a_selected, NONE)
    fun shape (v : string) = (a_shape, SOME v)
    fun size (v : string) = (a_size, SOME v)
    fun span (v : string) = (a_span, SOME v)
    fun src (v : string) = (a_src, SOME v)
    fun standby (v : string) = (a_standby, SOME v)
(* DEPRECATED
    fun start (v : string) = (a_start, SOME v) (* deprecated *)
*)
    fun style (v : string) = (a_style, SOME v)
    fun summary (v : string) = (a_summary, SOME v)
    fun tabindex (v : string) = (a_tabindex, SOME v)
    fun target (v : string) = (a_target, SOME v)
    fun text (v : string) = (a_text, SOME v)
    fun title (v : string) = (a_title, SOME v)
    fun type' (v : string) = (a_type, SOME v)
    fun usemap (v : string) = (a_usemap, SOME v)
    fun valign (v : string) = (a_valign, SOME v)
    fun value (v : string) = (a_value, SOME v)
    fun valuetype (v : string) = (a_valuetype, SOME v)
    fun version (v : string) = (a_version, SOME v)
(* DEPRECATED
    fun vlink (v : string) = (a_vlink, SOME v) (* deprecated *)
*)
    fun vspace (v : string) = (a_vspace, SOME v)
    fun width (v : string) = (a_width, SOME v)
    end (* local *)

  end (* HTML4Attrs *)
