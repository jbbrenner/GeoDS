Project_Path=$(pwd)
Outputs_Path="$Project_Path""/Model_Outputs"
echo $Outputs_Path
echo $Project_Path
echo "Choose directory"
read path
cd $path
ls
echo "Choose target file (high resolution DEM)"
read target
echo "Choose input file (low resolution climate data)"
read input
cdo griddes $target > target.grd
echo "Select an interpolation method"
read interp
var="$interp"",target.grd"
echo $var
command="cdo remap$var $input output.nc"
echo $command
$command
cp output.nc $Outputs_Path

