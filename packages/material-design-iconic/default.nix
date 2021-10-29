{ lib, fetchzip }:

let
  version = "2.2.0";
in fetchzip rec {
  name = "material-design-iconic-${version}";
  url = "https://github.com/zavoloklom/material-design-iconic-font/archive/${version}/material-design-iconic-font.zip";

  postFetch = ''
    mkdir -p $out/share/fonts/truetype
    unzip -j $downloadedFile \*.ttf -d $out/share/fonts/truetype
  '';

  sha256 = "53FaCgXftIYB4lV6suHYiYqlgaPeIYHx/Y3WV46e8g8=";

  meta = with lib; {
    homepage = "https://zavoloklom.github.io/material-design-iconic-font";
    description = "Full suite of material design icons";
    longDescription = ''
        Material Design Iconic Font is a full suite of official material design icons (created and maintained by Google),
        with additional community-designed and brands icons for easy scalable vector graphics on websites or desktop
    '';
    maintainers = with maintainers; [ ebn ];
    license = licenses.cc-by-sa-40;
    platforms = platforms.all;
  };
}
