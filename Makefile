################################################################################
##################### HAVK - A Unix-like Operating System. #####################
################################################################################
### This is the main Makefile for HAVK, which submakes the (as of now)
### two specific versions of HAVK. This was done so there would not be a
### large unreadable Makefile instead.
NAME:=HAVK
VERSION:=0.04-00
ROOT_DIR:=$(NAME)

# Build directory containing a subfolder for the specific HAVK version.
BUILD_DIR:=./build/
TARGET_DIR=$(BUILD_DIR)$(HAVK)/

# Disable the useless built-in implicit rules here. Speeds up `make` by a lot.
NO_BUILTIN=--no-builtin-rules --no-builtin-variables
MAKEFLAGS+=$(NO_BUILTIN)

ifneq ($(notdir $(shell pwd)), $(ROOT_DIR))
$(error Only run the Makefile from the root directory of HAVK's source)
endif

ifndef HAVK
$(error HAVK not defined. Pass a version like e.g. "make HAVK=xyz")
endif

.DEFAULT_GOAL: $(HAVK)
.PHONY: $(HAVK)
$(HAVK):
	if [ ! -d "$(TARGET_DIR)" ]; then mkdir -p $(TARGET_DIR); fi
	$(MAKE) -f Makefile.$(HAVK) $(NO_BUILTIN) VERSION=$(VERSION)

.PHONY: clean
clean:
	if [ ! -d "$(TARGET_DIR)" ]; then mkdir -p $(TARGET_DIR);\
	else rm -rf $(TARGET_DIR) && mkdir -p $(TARGET_DIR); fi

.PHONY: clean_all
clean_all:
	if [ ! -d "$(BUILD_DIR)" ]; then mkdir $(BUILD_DIR);\
	else rm -rf $(BUILD_DIR) && mkdir $(BUILD_DIR); fi
