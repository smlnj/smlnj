# changelog-macros.rb
#
# this file contains Ruby code to register the inline macros for processing the HISTORY.txt
# file using AciiDoctor.  See changelog-macros/extension.rb for the implementations.
#

RUBY_ENGINE == 'opal' ? (require 'changelog-macros/extension') : (require_relative 'changelog-macros/extension')

Asciidoctor::Extensions.register do
  if @document.basebackend? 'html'
    inline_macro BugRefInlineMacro
    inline_macro OwnerInlineMacro
  end
end
