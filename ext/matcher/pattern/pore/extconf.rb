require 'mkmf'
require 'rubygems'

have_header('assert.h')
have_header('stdbool.h')
have_header('stddef.h')
have_header('stdint.h')
have_header('stdio.h')
have_header('sys/types.h')

spec = Gem::GemPathSearcher.new.find('encoding/character/utf-8')
spec.require_paths.map{ |p| File.join(spec.full_gem_path, p) }.each do |path|
  $INCFLAGS << " -I#{path}"
end
  
$CFLAGS << ' -g'

have_header('encoding/character/utf-8/unicode.h')

def try_compiler_option(opt, &b)
  checking_for "‘#{opt}’ option to compiler" do
    if try_compile('', opt, &b)
      $CFLAGS += " #{opt}"
      true
    else
      false
    end
  end
end

try_compiler_option('-std=c99')
try_compiler_option('-Wall')
try_compiler_option('-W')
try_compiler_option('-Werror')
try_compiler_option('-Wcast-align')
# TODO: This is way too strict…
# try_compiler_option('-Wconversion')
try_compiler_option('-Wwrite-strings')
try_compiler_option('-Waggregate-return')
try_compiler_option('-Wmissing-prototypes')
try_compiler_option('-Wmissing-declarations')
try_compiler_option('-Wnested-externs')
try_compiler_option('-Wpointer-arith')
try_compiler_option('-Wundef')

create_makefile('matcher/pattern/pore/pore')
