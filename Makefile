# Copyright (c) 2010, Florian Rivoal
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY Florian Rivoal ''AS IS'' AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL Florian Rivoal BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# This is a base Makefile intended to suit many different projects. It needs to
# be customized to the specificities of each project, but provides
# infrastructure to make this task easier.  It currently supports C and C++
# Features:
# - Provides the following targets: all, clean, tags
# - Handles header file dependencies automatically
# - Builds from one or more source trees, with no restriction on subdirectories
# - Builds into a separate build tree, replicating the structure of the source
#   tree(s)
# - Supports putting generated source files in the build tree
#
# To use this Makefile, it should be sufficient to edit the section labeled
# "Project specific section", the rest is meant to be generic and cross
# project.

# -- User overridable commands --
MKDIR := mkdir -p
RM := rm -rf
AR := ar r

# -- User overridable path --
OUTDIR ?= $(if $(findstring $(base_path),.),out,$(base_path)/out)

# -- Auxiliary functions --

## Expands to a non empty string when the dependency files can be skipped.
## @param 1: List of targets for which dependency files are unneeded
deps_unneeded = $(and $(MAKECMDGOALS),$(findstring $(MAKECMDGOALS),$(filter $(1),$(MAKECMDGOALS))))

## Finds all the source files in specified directories and their subdirectories.
## Supported languages are c and c++.
## @param 1: list of directories to search source files in. Each directory's
##           path should be relative to base_path.
find_sources = $(shell cd $(base_path) && find $1 -name "*.cpp" -or -name "*.c")

## Expands to the list of object files, including their full path starting with
## OUTDIR..
## @param 1: List of c and c++ source files
obj_from_src = $(addprefix $(OUTDIR)/,$(patsubst %.c,%.o,$(filter %.c,$1)) $(patsubst %.cpp,%.o,$(filter %.cpp,$1)))

## Adds the directories to the include path of specified objects.
## @param 1: List of .o files
## @param 2: List of directories to add to the include path of param 1's .o
##           files, and of the matching .d files
add_inc_path = $(eval $1 $(1:.o=.d): CPPFLAGS += $(addprefix -I $(base_path)/,$2))

# -- Default rule --
.PHONY: all 
all: ;

# -- Top level rules --
.PHONY: tags clean
tags:
	ctags -R $(addprefix $(base_path)/,$(src_dirs))

clean:
	$(RM) $(OUTDIR) tags

###############################################################################
#                      ---- Project specific section ----                     #
# This section must define at least:
# - the 'base_path' variable, to which 'src_dirs' and the default value of
#   'OUTDIR' is relative.
# - the 'src_dirs' variable, which lists all the directories under which non
#   generated source code shall be found
# - the 'objects' variable, which lists all the .o files for which dependencies
#   should be computed
# - the prerequisites of the 'all' target
#
# Keep in mind that generated files should be stored under $(OUTDIR)/ 
base_path := .
src_dirs := src

sources := $(call find_sources,$(src_dirs))
objects := $(call obj_from_src,$(sources))
all: a.out

$(OUTDIR)/a.out: $(objects)
	$(LINK.c) -o $@ $^
#                  ---- End of project specific section ----                  #
###############################################################################

# -- Safety check --
ifneq ($(wildcard $(OUTDIR)),)
 ifeq ($(wildcard $(OUTDIR)/.exists),)
  $(error The output directory ($(OUTDIR)) exists but was not created by this makefile. Aborting to avoid overwriting or deleting existing files)
 endif
else
endif

# -- Dependency inclusions --
ifeq ($(call deps_unneeded,clean tags %/.exists),)
 include $(patsubst %.o,%.d,$(wildcard $(objects)))
 -include $(OUTDIR)/.exists
endif

# -- Generic build rules --
.PRECIOUS: %/.exists
%/.exists:
	$(MKDIR) $(@D)
	touch $@

VPATH = $(base_path) $(OUTDIR)
.SECONDEXPANSION:
%.o: %.c
$(OUTDIR)/%.d $(OUTDIR)/%.o: %.c $$(@D)/.exists
	$(COMPILE.c) -MMD -o $(@D)/$(*F).o $<

%.o: %.cpp
$(OUTDIR)/%.d $(OUTDIR)/%.o: %.cpp  $$(@D)/.exists
	$(COMPILE.cc) -MMD -MP -o $(@D)/$(*F).o $<
