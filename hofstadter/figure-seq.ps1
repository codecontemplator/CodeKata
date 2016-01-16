$d = 2
$dc = 1
$dcInit = 2
$x = 1
for($i=0; $i -lt 10; $i++)
{
    # yield
    Write-Host $x
    # add increment
    $x += $d
    # calculate next increment
    $d++
    $dc--
    if ($dc -eq 0) 
    {
        $d++
        $dcInit++
        $dc = $dcInit
    }
}
