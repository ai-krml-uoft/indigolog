set id 1
label .l_exog  -text "Exogenous Event:"
label .l_hist  -text "Past events:"
entry .entry_exog -width 30 -relief sunken -textvariable cmd
button .b_exit -text "Exit" -command { exit }

# build the interface
pack .l_exog -padx 1m -pady 1m
pack .entry_exog -padx 1m -pady 1m
pack .b_exit -padx 1m -pady 1m
pack .l_hist -padx 1m -pady 1m


button .b$id -command "puts stdout end_indi." -text end_indi
pack .b$id -fill x
# .b$id invoke

bind .entry_exog <Return> {
    # check if we have more tha 10 buttons, delete older buttons if so
    set id [expr $id + 1]
    if {$id > 9} {
    destroy .b[expr $id - 10]
    }
    button .b$id -command "puts stdout $cmd." -text $cmd
    pack .b$id -fill x
    .b$id invoke
    .entry_exog delete 0 end
}



