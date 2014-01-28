DEPLOY_TARGET=/Users/davidf/Projects/dafoster.net/dafoster.net/Code/assets/2014/incoming-game

# Halt upon any script errors
set -e

# Clean build
rm -rf build
./make.sh

# Clean deploy
rm -rf "$DEPLOY_TARGET"
cp -r build "$DEPLOY_TARGET"