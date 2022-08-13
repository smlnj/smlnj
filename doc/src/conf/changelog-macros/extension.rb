# extension.rb
#
# this file contains Ruby code to extend Asciidoctor for processing the HISTORY.txt
# file.
#

require 'asciidoctor/extensions' unless RUBY_ENGINE == 'opal'

include Asciidoctor

# An inline macro that generates links to bug reports
#
# Usage
#
#   bugref:123[]		-- renders as '#123'; links to bug report 123
#   bugref:123[feature]		-- renders as '#123'; links to feature request 123
#
class BugRefInlineMacro < Extensions::InlineMacroProcessor
  use_dsl

  named :bugref
  name_positional_attributes 'kind'

  def process parent, target, attrs
    num = target
    kind = attrs['kind']
    if parent.document.basebackend? 'html'
      atid = 215
      if kind.eql? "feature"
	atid = 218
      end
      target = "https://smlnj-gforge.cs.uchicago.edu/tracker/index.php?func=detail&aid=#{num}&group_id=33&atid=#{atid}"
      text = "<b>\##{num}</b>"
      parent.document.register :links, target
      %(#{(create_anchor parent, text, type: :link, target: target).convert})
    else
      "#{num}"
    end
  end
end

# An inline macro for adding an owner link to a change-log entry
#
# Usage
#
#   owner:url[name]		-- renders as 'name'; links to 'https:url'
#
class OwnerInlineMacro < Extensions::InlineMacroProcessor
  use_dsl

  named :owner
  name_positional_attributes 'name'

  def process parent, target, attrs
    owner_name = attrs['name']
    if parent.document.basebackend? 'html'
      if owner_name.empty?
	owner_name = "Unknown"
      end
      if target.empty?
	target = "https://smlnj.org"
      else
	target = "https://" + target
      end
      text = owner_name
      parent.document.register :links, target
      %(#{(create_anchor parent, text, type: :link, target: target).convert})
    else
      owner_name
    end
  end
end
