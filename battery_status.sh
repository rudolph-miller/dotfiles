if [ `type -p acpi` ]; then
  if [ "`acpi -b | grep -oE Discharging`" = "Discharging" ] ; then
    echo -n \#\[fg=yellow, bold\]
    echo -n "Discharging: "
  else
    echo -n \#\[fg=green,bold\]
    echo -n "Charging: "
  fi

  echo `acpi -b | grep -oE \[0-9\]+%`
else
  echo ""
fi
