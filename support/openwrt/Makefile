#
# Copyright (C) 2009 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#

include $(TOPDIR)/rules.mk

PKG_NAME:=erlang
PKG_VERSION:=R15B03
PKG_RELEASE:=2

PKG_SOURCE:=otp_src_$(PKG_VERSION)-1.tar.gz
PKG_SOURCE_URL:= http://www.erlang.org/download/ \
	http://erlang.mirror.su.se/	
PKG_MD5SUM:=eccd1e6dda6132993555e088005019f2

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
  PROVIDES:= erlang-erts=5.9.3.1 erlang-kernel=2.15.3 erlang-sasl=2.2.1 erlang-stdlib=1.18.3 erlang-crypto=2.2
endef

define Package/erlang/description
$(call Package/erlang/Default/description)
 .
 This package contains the runtime implementation and a minimal set of 
 modules (erts, kernel, sasl & stdlib).
endef


define Package/erlang-observer
$(call Package/erlang/Default)
  TITLE:=Observer support
  VERSION:=1.0
  DEPENDS+= +erlang
endef

define Package/erlang-observer/description
$(call Package/erlang/Default/description)
 .
 Observer, tools for tracing and investigation of distributed systems.
endef


define Package/erlang-runtime_tools
$(call Package/erlang/Default)
  TITLE:=Runtime tools
  VERSION:=1.8.7
  DEPENDS+= +erlang
endef

define Package/erlang-runtime_tools/description
$(call Package/erlang/Default/description)
 .
 Runtime tools, tools to include in a production system.
endef

define Package/erlang-xmerl
$(call Package/erlang/Default)
  TITLE:=XML parser
  VERSION:=1.3.2
  DEPENDS+= +erlang
endef

define Package/erlang-xmerl/description
$(call Package/erlang/Default/description)
 .
 XML parser for 1.0 XML documents.
endef


# Host

HOST_CONFIGURE_ARGS += \
	--disable-hipe \
	--disable-smp-support \
	--without-javac \
	--without-ssl

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
	--without-javac
#	--without-java \
#	--with-ssl="$(STAGING_DIR)/usr" \
#	--enable-dynamic-ssl-lib

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
	for m in erts kernel sasl stdlib crypto; do \
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


define BuildModule

  define Package/erlang-$(1)/install
	$(INSTALL_DIR) $$(1)/usr/lib/erlang/lib
	for m in $(2); do \
		$(CP) $(PKG_INSTALL_DIR)/usr/lib/erlang/lib/$$$$$$$$m-* $$(1)/usr/lib/erlang/lib/ ; \
		rm -rf $$(1)/usr/lib/erlang/lib/$$$$$$$$m-*/{examples,priv/obj,src} ; \
	done
  endef

  $$(eval $$(call BuildPackage,erlang-$(1)))

endef

$(eval $(call HostBuild))
$(eval $(call BuildPackage,erlang))
$(eval $(call BuildModule,observer,observer))
$(eval $(call BuildModule,runtime_tools,runtime_tools))
$(eval $(call BuildModule,xmerl,xmerl))
