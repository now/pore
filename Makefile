
SHELL = /bin/sh

#### Start of system configuration section. ####

srcdir = .
topdir = /usr/lib64/ruby/1.8/x86_64-linux
hdrdir = $(topdir)
VPATH = $(srcdir):$(topdir):$(hdrdir)
prefix = $(DESTDIR)/usr
exec_prefix = $(prefix)
sitedir = $(DESTDIR)/usr/lib64/ruby/site_ruby
rubylibdir = $(libdir)/ruby/$(ruby_version)
archdir = $(rubylibdir)/$(arch)
sbindir = $(exec_prefix)/sbin
datadir = $(DESTDIR)/usr/share
includedir = $(prefix)/include
infodir = $(DESTDIR)/usr/share/info
sysconfdir = $(DESTDIR)/etc
mandir = $(DESTDIR)/usr/share/man
libdir = $(DESTDIR)/usr/lib64
sharedstatedir = $(prefix)/com
oldincludedir = $(DESTDIR)/usr/include
sitearchdir = $(sitelibdir)/$(sitearch)
bindir = $(exec_prefix)/bin
localstatedir = $(DESTDIR)/var/lib
sitelibdir = $(DESTDIR)/usr/lib64/ruby/site_ruby/1.8
libexecdir = $(exec_prefix)/libexec

CC = x86_64-pc-linux-gnu-gcc
LIBRUBY = $(LIBRUBY_SO)
LIBRUBY_A = lib$(RUBY_SO_NAME)-static.a
LIBRUBYARG_SHARED = -Wl,-R -Wl,$(libdir) -L$(libdir) -L. -l$(RUBY_SO_NAME)
LIBRUBYARG_STATIC = -l$(RUBY_SO_NAME)-static

RUBY_EXTCONF_H = 
CFLAGS   =  -fPIC -march=k8 -O2 -pipe -fno-strict-aliasing  -fPIC -I$(sitelibdir)/$(sitearch) -g -std=c99 -Wall -W -Wwrite-strings -Waggregate-return -Wmissing-prototypes -Wmissing-declarations -Wnested-externs -Wundef -Wpointer-arith -Wcast-align -Werror -Wshadow 
INCFLAGS = -I. -I$(topdir) -I$(hdrdir) -I$(srcdir) -I$(sitelibdir)/$(sitearch)
CPPFLAGS = -DHAVE_ASSERT_H -DHAVE_STDBOOL_H -DHAVE_STDDEF_H -DHAVE_STDINT_H -DHAVE_STDIO_H -DHAVE_SYS_TYPES_H -DHAVE_NED_UNICODE_H 
CXXFLAGS = $(CFLAGS) 
DLDFLAGS =   
LDSHARED = $(CC) -shared
AR = x86_64-pc-linux-gnu-ar
EXEEXT = 

RUBY_INSTALL_NAME = ruby18
RUBY_SO_NAME = ruby18
arch = x86_64-linux
sitearch = x86_64-linux
ruby_version = 1.8
ruby = /usr/bin/ruby18
RUBY = $(ruby)
RM = rm -f
MAKEDIRS = mkdir -p
INSTALL = /bin/install -c
INSTALL_PROG = $(INSTALL) -m 0755
INSTALL_DATA = $(INSTALL) -m 644
COPY = cp

#### End of system configuration section. ####

preload = 

libpath = $(libdir)
LIBPATH =  -L'$(libdir)' -Wl,-R'$(libdir)'
DEFFILE = 

CLEANFILES = 
DISTCLEANFILES = 

extout = 
extout_prefix = 
target_prefix = /ned
LOCAL_LIBS = 
LIBS = $(LIBRUBYARG_SHARED)  -lpthread -ldl -lcrypt -lm   -lc
SRCS = ast.c compile.c execute.c match.c mempool.c parse.c patternmatcher.c
OBJS = ast.o compile.o execute.o match.o mempool.o parse.o patternmatcher.o
TARGET = patternmatcher
DLLIB = $(TARGET).so
EXTSTATIC = 
STATIC_LIB = 

RUBYCOMMONDIR = $(sitedir)$(target_prefix)
RUBYLIBDIR    = $(sitelibdir)$(target_prefix)
RUBYARCHDIR   = $(sitearchdir)$(target_prefix)

TARGET_SO     = $(DLLIB)
CLEANLIBS     = $(TARGET).so $(TARGET).il? $(TARGET).tds $(TARGET).map
CLEANOBJS     = *.o *.a *.s[ol] *.pdb *.exp *.bak

all:		$(DLLIB)
static:		$(STATIC_LIB)

clean:
		@-$(RM) $(CLEANLIBS) $(CLEANOBJS) $(CLEANFILES)

distclean:	clean
		@-$(RM) Makefile $(RUBY_EXTCONF_H) conftest.* mkmf.log
		@-$(RM) core ruby$(EXEEXT) *~ $(DISTCLEANFILES)

realclean:	distclean
install: install-so install-rb

install-so: $(RUBYARCHDIR)
install-so: $(RUBYARCHDIR)/$(DLLIB)
$(RUBYARCHDIR)/$(DLLIB): $(DLLIB)
	$(INSTALL_PROG) $(DLLIB) $(RUBYARCHDIR)
install-rb: pre-install-rb install-rb-default
install-rb-default: pre-install-rb-default
pre-install-rb: Makefile
pre-install-rb-default: Makefile
pre-install-rb: $(RUBYARCHDIR)
install-rb: $(RUBYARCHDIR)/patternmatcher.rb
$(RUBYARCHDIR)/patternmatcher.rb: $(srcdir)/patternmatcher.rb
	$(INSTALL_DATA) $(srcdir)/patternmatcher.rb $(@D)
$(RUBYARCHDIR):
	$(MAKEDIRS) $@

site-install: site-install-so site-install-rb
site-install-so: install-so
site-install-rb: install-rb

.SUFFIXES: .c .m .cc .cxx .cpp .C .o

.cc.o:
	$(CXX) $(INCFLAGS) $(CPPFLAGS) $(CXXFLAGS) -c $<

.cxx.o:
	$(CXX) $(INCFLAGS) $(CPPFLAGS) $(CXXFLAGS) -c $<

.cpp.o:
	$(CXX) $(INCFLAGS) $(CPPFLAGS) $(CXXFLAGS) -c $<

.C.o:
	$(CXX) $(INCFLAGS) $(CPPFLAGS) $(CXXFLAGS) -c $<

.c.o:
	$(CC) $(INCFLAGS) $(CPPFLAGS) $(CFLAGS) -c $<

$(DLLIB): $(OBJS)
	@-$(RM) $@
	$(LDSHARED) $(DLDFLAGS) $(LIBPATH) -o $@ $(OBJS) $(LOCAL_LIBS) $(LIBS)



###
ast.o: ast.c mempool.h ast.h private.h
compile.o: compile.c private.h mempool.h ast.h compile.h
execute.o: execute.c mempool.h private.h ast.h parse.h compile.h \
  execute.h match.h
match.o: match.c private.h match.h
mempool.o: mempool.c private.h mempool.h
parse.o: parse.c mempool.h ast.h parse.h private.h
patternmatcher.o: patternmatcher.c mempool.h private.h ast.h parse.h \
  compile.h execute.h match.h
tags: TAGS
	ctags -f TAGS --declarations --globals -T -w $^
docs:
	rdoc18 --charset utf-8

TEX_SOURCES := $(addsuffix .tex,$(addprefix ../../../../doc/thesis/thesis/sources/patternmatcher/,$(wildcard *.[ch])))

condocs: $(TEX_SOURCES)

../../../../doc/thesis/thesis/sources/patternmatcher/%.tex: %
	{ echo "% This is a generated file.  Please don’t make changes to it."; \
	  echo; \
	  echo "\startcomponent" `echo $@ | sed 's:../../../../doc/thesis/::'`; \
	  echo; \
	  echo "\project masters-project"; \
	  echo "\product thesis"; \
	  echo; \
	  echo; \
	  echo; \
	  condoc.rb $<; \
	  echo; \
	  echo; \
	  echo "\stopcomponent"; \
	} > $@

.PHONY: tags docs condocs
