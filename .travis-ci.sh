
OPAM_DEPENDS="ocamlfind ocamlfind ounit pareto"
APT_DEPENDS="gsl-bin libgsl0-dev liblapack-dev libblas-dev gfortran"
APT_OCAML="ocaml ocaml-native-compilers camlp4-extra opam aspcud curl"

COVERAGE=1

case "$OCAML_VERSION,$OPAM_VERSION" in
  4.00.1,1.0.0) ppa=avsm/ocaml40+opam10  ;;
  4.00.1,1.1.0) ppa=avsm/ocaml40+opam11  ;;
  4.00.1,1.1.1) ppa=avsm/ocaml40+opam111 ;;
  4.01.0,1.0.0) ppa=avsm/ocaml41+opam10  ;;
  4.01.0,1.1.0) ppa=avsm/ocaml41+opam11  ;;
  4.01.0,1.1.1) ppa=avsm/ocaml41+opam111 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

# setup OPAM
sudo add-apt-repository -y ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq $APT_OCAML $APT_DEPENDS
opam init -y
eval `opam config env`
opam update -y
opam install ${OPAM_DEPENDS} -y

# setup ocverall support via modified bisect
if [ 1 -eq $COVERAGE ] ; then
  wget http://sagotch.fr/Bisect.tar.gz
  tar -xvf Bisect.tar.gz
  cd Bisect
  chmod +x configure
  ./configure
  make all
  sudo make install
  cd ..
fi

# make/test application
make
if [ 1 -eq $COVERAGE ] ; then
  make coverage
else
  make test.native
fi
make test.byte

# install/test application linking
#make install
#make test
#./test/test.native

# make documentation
#make phylocaml.html

# run toplevel with phylocaml TODO
#echo "#use \"topfind\";;
#      #require \"phylocaml\";;
#      open Phylocaml.Tree;;" > test.ml
#ocaml test.ml

#make and execute each app in app/ TODO
#make apps.all
#make apps.test.native
#./apps/test.native
