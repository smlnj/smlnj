#! /usr/bin/env python
# ______________________________________________________________________
"""helper.py

Quick helper script for some minor code generation tasks.  Requires
the BeautifulSoup library.
"""
# ______________________________________________________________________
# Module imports

from BeautifulSoup import BeautifulSoup

# ______________________________________________________________________
# Function definitions

def element_data_to_tuple_str ((tag_name, start_tag_data, end_tag_data,
                                empty_data, depr_data, dtd_data, desc_data)):
    otag_ctor = u"START%s" % tag_name
    if end_tag_data == u"F":
        etag_ctor = u"NONE"
    else:
        etag_ctor = u"SOME END%s" % tag_name
    return u'("%s", %s, %s)' % (tag_name, otag_ctor, etag_ctor)

# ______________________________________________________________________

def element_data_to_string_fun ((tag_name, start_tag_data, end_tag_data,
                                 empty_data, depr_data, dtd_data, desc_data)):
    print (u'| tokToString (START%s payload) = "START%s " ^ (payloadToString '
           'payload)' % (tag_name, tag_name))
    if end_tag_data != u"F":
        print u'| tokToString END%s = "END%s"' % (tag_name, tag_name)

# ______________________________________________________________________

def element_data_to_production ((tag_name, start_tag_data, end_tag_data,
                                 empty_data, depr_data, dtd_data, desc_data)):
    end_str = u";"
    if end_tag_data != u"F":
        end_str = u"END%s ;" % tag_name
    if empty_data != u"E":
        end_str = u"XXX %s" % end_str
    print u"%s : START%s %s" % (tag_name.lower(), tag_name, end_str)
    print

# ______________________________________________________________________
# Main routine

def main ():
    elements_doc = BeautifulSoup(open("tests/elements.html").read(),
                                 convertEntities = BeautifulSoup.HTML_ENTITIES)
    # Skip the header row.
    crnt_element_row = elements_doc.table.tr.findNextSibling("tr")
    strict_list = []
    loose_list = []
    frameset_list = []
    while crnt_element_row is not None:
        cols = crnt_element_row.findAll("td")
        assert len(cols) == 7
        element_data = tuple([cols[0].a.string.strip()] +
                             [table_entry.string.strip()
                              for table_entry in cols[1:]])
        (tag_name, start_tag_data, end_tag_data, empty_data, depr_data,
         dtd_data, desc_data) = element_data
        print u"| START%s of token_payload" % tag_name
        if end_tag_data != u"F":
            print u"| END%s" % tag_name
        else:
            print u"(* No END tag for %s element. *)" % tag_name
        if dtd_data == u'':
            strict_list.append(element_data)
        elif dtd_data == u'F':
            frameset_list.append(element_data)
        else:
            assert dtd_data == u'L'
            loose_list.append(element_data)
        crnt_element_row = crnt_element_row.findNextSibling("tr")
    element_data_list = strict_list + loose_list + frameset_list
    element_data_list.sort()
    print
    print "val strict_tuple_list = ["
    print u",\n".join((element_data_to_tuple_str(elem_data)
                       for elem_data in strict_list))
    print "]"
    print
    print "val loose_tuple_list = ["
    print u",\n".join((element_data_to_tuple_str(elem_data)
                       for elem_data in loose_list))
    print "]"
    print
    print "val frameset_tuple_list = ["
    print u",\n".join((element_data_to_tuple_str(elem_data)
                       for elem_data in frameset_list))
    print "]"
    print
    for element_data in element_data_list:
        element_data_to_string_fun(element_data)
    print
    for element_data in element_data_list:
        element_data_to_production(element_data)

# ______________________________________________________________________

if __name__ == "__main__":
    main()

# ______________________________________________________________________
# End of helper.py
