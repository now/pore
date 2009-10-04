# contents: Initialization of the Matcher::Pattern::Pore library.
#
# Copyright © 2005,2006 Nikolai Weibull <now@bitwi.se>

require 'encoding/character/utf-8'
require 'matcher/pattern/pore/pore.so'

class Matcher::Pattern::Pore::Match
  def length
    self.end - self.begin + 1
  end
  alias size length
end
