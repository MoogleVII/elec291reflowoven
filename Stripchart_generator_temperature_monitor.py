import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import sys, time, math, serial, smtplib

ser = serial.Serial(
    port='COM9',
    baudrate=115200,
    parity=serial.PARITY_NONE,
    stopbits=serial.STOPBITS_ONE,
    bytesize=serial.EIGHTBITS
)
ser.isOpen()    
xsize=50
   
  
state5 = 0
   
def data_gen():
    t = data_gen.t
    while True:
       t+=1
       val=ser.readline()
       
	   if val > 200
	       state5 = 1
		   server = smtplib.SMTP('smtp.gmail.com', 587)
		   server.starttls()
           server.login("flow.oven@gmail.com", "reflowdat") 
       if state5 == 1
	       if val < 200        
		       msg = "SOlDER HAS BEEN REFLOWED. PLEASE COLLECT. HELP ME"
               subject = "HELLO"
			   server.sendmail("flow.oven@gmail.com", "davin.birdi@hotmail.ca", msg)
			   server.quit()	   
	   
	   yield t, val

def run(data):
    # update the data
    t,y = data
    if t>-1:
        xdata.append(t)
        ydata.append(y)
       # if t>xsize: # Scroll to the left.
        #    ax.set_xlim(t-xsize, t)
        line.set_data(xdata, ydata)

    return line,

def on_close_figure(event):
    sys.exit(0)

data_gen.t = -1
fig = plt.figure()
fig.canvas.mpl_connect('close_event', on_close_figure)
ax = fig.add_subplot(111)
line, = ax.plot([], [], lw=2)
ax.set_ylim(0, 250)
ax.set_xlim(0, 2000)
ax.grid()
xdata, ydata = [], []

# Important: Although blit=True makes graphing faster, we need blit=False to prevent
# spurious lines to appear when resizing the stripchart.
ani = animation.FuncAnimation(fig, run, data_gen, blit=False, interval=10, repeat=False)
plt.show()
