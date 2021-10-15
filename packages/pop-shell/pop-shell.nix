{ stdenv, lib, fetchFromGitHub, nodejs, nodePackages, glib }:

stdenv.mkDerivation rec {
  pname = "gnome-shell-extension-pop-shell";
  version = "2021-10-13";

  src = fetchFromGitHub {
    owner = "pop-os";
    repo = "shell";
    rev = "e7f30249a18317de3a54dc185e3a547ee4a74022";
    sha256 = "0z9dhcgm96bp7b15vng06bsv6dbyas4rxg1vyhzmvpdnxz0d2hjd";
  };

  uuid = "pop-shell@system76.com";

  nativeBuildInputs = [ glib nodePackages.typescript];

  makeFlags = [ 
    "INSTALLBASE=$(out)/share/gnome-shell/extensions"
    "PLUGIN_BASE=$(out)/lib/pop-shell/launcher"
    "SCRIPTS_BASE=$(out)/lib/pop-shell/scripts"
  ];

  postInstall = ''

    mkdir -p $out/share/gsettings-schemas/pop-shell-${version}/glib-2.0

    schemadir=${glib.makeSchemaPath "$out" "${pname}-${version}"}
    mkdir -p $schemadir
    cp -r $out/share/gnome-shell/extensions/$uuid/schemas/* $schemadir
  
  '';

  meta = with lib; {
    description = "i3wm-like keyboard-driven layer for GNOME Shell";
    homepage = "https://github.com/pop-os/shell";
    license = licenses.gpl3;
    maintainers = with maintainers; [ ebn ];
    platforms = platforms.linux;
  };
}
