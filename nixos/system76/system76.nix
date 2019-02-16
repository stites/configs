{ stdenv, fetchgit, fetchpatch, kernel }:

stdenv.mkDerivation {
  name = "system76-${kernel.version}";

  src = fetchgit {
    url = "git://github.com/pop-os/system76-dkms.git";
    rev = "93caafa56aa28437bfc6251b4472a63139d05cbd";
    sha256 = "1bjl6w5hnlprb1n3823g30wwfwxwq20bwr8lwj5sibcgyh1n443y";
  };

  hardeningDisable = [ "pic" ];

  nativeBuildInputs = kernel.moduleBuildDependencies;

  preBuild = ''
    sed -e "s@/lib/modules/\$(.*)@${kernel.dev}/lib/modules/${kernel.modDirVersion}@" -i Makefile
  '';

  installPhase = ''
    mkdir -p $out/lib/modules/${kernel.modDirVersion}/misc
    cp system76.ko $out/lib/modules/${kernel.modDirVersion}/misc
  '';

  meta = {
    maintainers = [/* future you! */];
    platforms = stdenv.lib.platforms.linux;
    description = "System-specific kernel for System76 hardware";
  };
}