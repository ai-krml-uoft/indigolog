"""Exogenous action GUI interface via Python Tkinter

Sebastian Sardina ssardina@gmail.com - Dec 12, 2024

Allows to submit exogenous actions by printing to stdout and adding buttons to submit them easily.

$ python src/job_bpmn/exog.py --events end_indi open_door(1)
"""

import tkinter as tk
import argparse
# a good tutorial on tkinter: https://www.pythontutorial.net/tkinter/tkinter-window/

WIDTH = 300
HEIGHT = 200
MAX_NO_BUTTONS = 15
past_events = []
no_fixed_events = 0


def main(exog_events=[]):
    def print_exog(event):
        # called when typing in the entry widget to issue an exogenous event
        # print exogenous action text as term (with full stop) and add button as necessary
        global past_events
        entry_text = exog_entry.get()
        exog_entry.delete(0, tk.END)
        print(entry_text + "." if entry_text[-1] != "." else entry_text)

        if entry_text not in [x["text"] for x in past_events]:
            if len(past_events) < MAX_NO_BUTTONS - no_fixed_events:
                add_button(entry_text if entry_text[-1] != "." else entry_text[:-1])
            else:
                past_events[0].destroy()
                past_events = past_events[1:]
                add_button(entry_text if entry_text[-1] != "." else entry_text[:-1])

    def add_button(exog_action, fixed=False):
        # add a button with a fixed text to submit an event
        global past_events

        # btn = tk.Button(root, text=exog_action, command=lambda : report_button(tk.StringVar(value=exog_action)))
        btn = tk.Button(root, text=exog_action, command=lambda : print(exog_action + "." if exog_action[-1] != "." else exog_action))
        btn.pack(pady=(5, 0))
        root.geometry(f"{WIDTH}x{HEIGHT + (len(past_events)+no_fixed_events) * 35}")
        # root.geometry(f"{WIDTH}x{root.winfo_screenmmheight()}")

        if not fixed:
            past_events.append(btn)

    # create Tkinter root application
    root = tk.Tk()
    root.geometry(f"{WIDTH}x{HEIGHT}+50+50")
    root.minsize(WIDTH, HEIGHT)
    root.title("Exogenous Events")

    # create entry widget to type exogenous events
    # Create a Label widget to display the text
    tk.Label(root, text="Exogenous Action").pack()
    exog_entry = tk.Entry(root, width=20)
    exog_entry.pack(pady=(0, 10))
    exog_entry.bind("<Return>", print_exog) # bind the entry widget to the function

    # Add fixed buttons
    # add_button("end_indi", fixed=True)
    global no_fixed_events
    for x in exog_events:
        no_fixed_events += 1
        add_button(x, fixed=True)

    tk.Label(root, text="Past events:").pack()

    root.mainloop()



if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Exogenous Events GUI")
    parser.add_argument("--events", nargs="*", default=[], help="List of initial exogenous events")
    args = parser.parse_args()

    # main(["end_indi"])
    main(args.events)
