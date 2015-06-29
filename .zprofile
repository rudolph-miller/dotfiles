case $TERM in
  linux)
    if [ -c /dev/fb0 ]; then
      fbterm -- tmux
      exit
    fi
    ;;
esac
