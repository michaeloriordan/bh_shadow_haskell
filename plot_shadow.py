import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

plt.rc('text', usetex=True)
plt.rc('font', family='serif', serif='cm10')
plt.rc('path', simplify=False)
plt.rcParams['text.latex.preamble']=[r'\usepackage{amsmath}']
    
def read_txt(filename):
    df = pd.read_csv(filename, header=None, skipinitialspace=True, 
                     delim_whitespace=True, engine='c')
    return df.values.T
    
def init_fig(xlabel, ylabel):
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.xaxis.label.set_fontsize(24)
    ax.yaxis.label.set_fontsize(24)
    ax.tick_params(axis='both', which='major', labelsize=16)
    plt.tight_layout()
    return fig, ax

def plot_shadow(filename, plotname, nx, ny):
    d = read_txt(filename)
    x, y, r, th, phi, esc = d.reshape(d.shape[0], nx, ny)

    xlabel = '$x/r_g$'
    ylabel = '$y/r_g$'
    fig, ax = init_fig(xlabel, ylabel)

    ax.imshow(esc, extent=[x.min(), x.max(), y.min(), y.max()], 
              cmap=plt.get_cmap('gray'), interpolation='none')

    fig.savefig(plotname+'.png', format='png', dpi=300)
    plt.close(fig)

if __name__ == '__main__':
    filename = 'data.txt'
    plotname = 'shadow'
    nx = 64
    ny = 64
    plot_shadow(filename, plotname, nx, ny)
