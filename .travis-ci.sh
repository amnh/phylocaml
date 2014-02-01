OPAM_DEPENDS="ocamlfind ocamlfind ounit pareto"
APT_DEPENDS="gsl-bin libgsl0-dev liblapack-dev libblas-dev gfortran"
APT_OCAML="ocaml ocaml-native-compilers camlp4-extra opam aspcud"

case "$OCAML_VERSION,$OPAM_VERSION" in
  4.00.1,1.0.0) ppa=avsm/ocaml40+opam10  ;;
  4.00.1,1.1.0) ppa=avsm/ocaml40+opam11  ;;
  4.00.1,1.1.1) ppa=avsm/ocaml40+opam111 ;;
  4.01.0,1.0.0) ppa=avsm/ocaml41+opam10  ;;
  4.01.0,1.1.0) ppa=avsm/ocaml41+opam11  ;;
  4.01.0,1.1.1) ppa=avsm/ocaml41+opam111 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

sudo add-apt-repository -y ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq $APT_OCAML $APT_DEPENDS

# setup OPAM
opam -y init
eval `opam config env`
opam -y update
opam -y install ${OPAM_DEPENDS}

# make/test application
make
make test.native
./test.native

# install/test application linking
make install
make extests
./test/test.native

# make documentation
make phylocaml.html

# run toplevel with phylocaml TODO
#echo "#use \"topfind\";;
#      #require \"phylocaml\";;
#      open Phylocaml.Tree;;" > test.ml
#ocaml test.ml

#make and execute each app in app/ TODO
#make apps.all
#make apps.test.native
#./apps/test.native
