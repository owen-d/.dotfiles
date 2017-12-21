if [[ $(uname -s) == Linux ]]
  then
    sleep 1
    # make capsLock behave like Ctrl:
    setxkbmap -option "ctrl:nocaps" -option "caps:ctrl_modifier"

    # make short-pressed Ctrl behave like Escape:
    xcape -e 'Control_L=Escape'
fi

