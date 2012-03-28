#
# Copyright (C) 2009 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#

include $(TOPDIR)/rules.mk

PKG_NAME:=erlang
PKG_VERSION:=R15B
PKG_RELEASE:=5

PKG_SOURCE:=otp_src_$(PKG_VERSION).tar.gz
PKG_SOURCE_URL:= http://www.erlang.org/download/ \
	http://erlang.mirror.su.se/	
PKG_MD5SUM:=dd6c2a4807551b4a8a536067bde31d73

PKG_BUILD_DEPENDS:=erlang/host openssl

HOST_BUILD_DIR:=$(BUILD_DIR_HOST)/otp_src_$(PKG_VERSION)
PKG_BUILD_DIR:=$(BUILD_DIR)/otp_src_$(PKG_VERSION)

include $(INCLUDE_DIR)/host-build.mk
include $(INCLUDE_DIR)/package.mk

define Package/erlang/Default
  SUBMENU:=Erlang
  SECTION:=lang
  CATEGORY:=Languages
  TITLE:=Erlang/OTP programming language
  URL:=http://www.erlang.org/
endef

define Package/erlang/Default/description
 Erlang/OTP is a general-purpose programming language and runtime 
 environment. Erlang has built-in support for concurrency, distribution 
 and fault tolerance.
endef


define Package/erlang
$(call Package/erlang/Default)
  DEPENDS+= +libncurses +librt +zlib
  PROVIDES:= erlang-erts=5.9 erlang-kernel=2.15 erlang-sasl=2.2 erlang-stdlib=1.18
endef

define Package/erlang/description
$(call Package/erlang/Default/description)
 .
 This package contains the runtime implementation and a minimal set of 
 modules (erts, kernel, sasl & stdlib).
endef

# Host

HOST_CONFIGURE_ARGS += \
	--disable-hipe \
	--disable-smp-support \
	--without-java \
	--without-ssl \
	--enable-darwin-64bit

HOST_CONFIGURE_VARS += \
	ac_cv_prog_JAVAC="" \
	ac_cv_prog_javac_ver_1_2=no

define Host/Compile
	$(MAKE) -C $(HOST_BUILD_DIR) all
endef

define Host/Install
	$(MAKE) -C $(HOST_BUILD_DIR) install
endef


# Target

CONFIGURE_ARGS += \
	--disable-hipe \
	--disable-smp-support \
	--without-java \
	--with-ssl="$(STAGING_DIR)/usr" \
	--enable-dynamic-ssl-lib

CONFIGURE_VARS += \
	SHLIB_LD="$(TARGET_CC)" \
	TARGET_ARCH="$(TARGET_ARCH)" \
	ac_cv_prog_RX_LD="$(TARGET_LD)" \
	ac_cv_prog_JAVAC="" \
	ac_cv_prog_javac_ver_1_2=no \
	ac_cv_func_getaddrinfo=no \
	ac_cv_func_mmap_fixed_mapped=yes \
	ac_cv_path_WX_CONFIG_PATH=no

EXTRA_LDFLAGS+=-lz

define Build/Compile
	$(MAKE) -C $(PKG_BUILD_DIR) \
		noboot
	$(MAKE) -C $(PKG_BUILD_DIR) \
		INSTALL_PREFIX="$(PKG_INSTALL_DIR)" \
		install
endef

define Package/erlang/install
	$(INSTALL_DIR) $(1)/usr/bin
	for f in epmd erl erlc escript run_erl; do \
		$(CP) $(PKG_INSTALL_DIR)/usr/bin/$$$$f $(1)/usr/bin/ ; \
	done
	$(INSTALL_DIR) $(1)/usr/lib/erlang/bin
	for f in erl erlc escript run_erl start start.boot start.script start_clean.boot start_erl start_sasl.boot to_erl; do \
		$(CP) $(PKG_INSTALL_DIR)/usr/lib/erlang/bin/$$$$f $(1)/usr/lib/erlang/bin/ ; \
	done
	$(INSTALL_DIR) $(1)/usr/lib/erlang/lib
	for m in erts kernel sasl stdlib; do \
		$(CP) $(PKG_INSTALL_DIR)/usr/lib/erlang/lib/$$$$m-* $(1)/usr/lib/erlang/lib/ ; \
		rm -rf $(1)/usr/lib/erlang/lib/$$$$m-*/examples ; \
		rm -rf $(1)/usr/lib/erlang/lib/$$$$m-*/src ; \
	done
	$(INSTALL_DIR) $(1)/usr/lib/erlang
	$(CP) $(PKG_INSTALL_DIR)/usr/lib/erlang/erts-* $(1)/usr/lib/erlang/
	rm -rf $(1)/usr/lib/erlang/erts-*/{doc,include,lib,man,src}
	rm -rf $(1)/usr/lib/erlang/erts-*/bin/*.src
	$(INSTALL_DIR) $(1)/usr/lib/erlang/releases
	$(CP) $(PKG_INSTALL_DIR)/usr/lib/erlang/releases/* $(1)/usr/lib/erlang/releases/
	$(SED) 's,%ERL_ROOT%,/usr/lib/erlang,g' \
		$(1)/usr/lib/erlang/releases/RELEASES.src
	mv -f $(1)/usr/lib/erlang/releases/RELEASES.src \
		$(1)/usr/lib/erlang/releases/RELEASES
	for f in bin/erl bin/start erts-*/bin/erl erts-*/bin/start; do \
		$(SED) 's,^\(ROOTDIR\)=.*,\1=/usr/lib/erlang,g' \
			$(1)/usr/lib/erlang/$$$$f ; \
	done
endef


$(eval $(call HostBuild))
$(eval $(call BuildPackage,erlang))
