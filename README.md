# Small Project for a Computational Physics class during my Bachelors
An exercise I completed for a Computational Physics class. Using a molecular dynamics simulation to estimate the melting temperature of a crystal.
## Short explanation
The assignment was only to write some code and plot some results, it did not include a requirement to write a paper, thus I only uploaded the code here.
The instructions were given in this script (https://comphys.unibas.ch/TEACH/CP/course.pdf) under the title: "Determination of the melting temperature of silicon by a molecular dynamics simulation".
Two sets of input data (initial positions and velocities) were provided, they are contained in the files hot.dat and cold.dat found in the following tar-file (https://comphys.unibas.ch/TEACH/WS13/Material.tar). Both consisting of a liquid, at different temperature, sandwiched between two crystal plates. By running an MD simulation using the EDIP force field with both initial data and letting them equilibrate an estimate for the melting temperature was found.
The Project was subdivided in three distinct tasks (paraphrased):
1. Implement a Velocity Verlet routine and test it by simulating an harmonic oscillator and see that the energy is conserved.
2. Run a short MD simulation of both input files provided and verify that there too, the energy is conserved. Add code to calculate the instantaneous temperature.
3. Run MD simulations on both input files until equilibration and plot the instantaneous temperatures for both as well as a running average. Use this to estimate the melting temperature.

Each task has it's own folder with all relevant files. In each task folder there's a sub-folder called SOURCE containing all the source code, including a gnu-plot script used to generate the plots.
### Code and Input data used
As per the instructions the following two pieces of code, and input data, were included from the material provided (.tar file linked above: https://comphys.unibas.ch/TEACH/WS13/Material.tar):
- MD.f90 (modified in terms of formatting, and some code added as per the instructions)
- bazant_lib.f90 (used without modification)
- hot.dat (used without modification)
- cold.dat (used without modification)
