$CodeQLPath = 'C:\src\code_1\target\intree\codeql\codeql.cmd'

function RunQuery {
    param(
        [string] $Query,
        [string] $Database,
        [string] $Library
    )

    $FullQuery = Join-Path $Library $Query
    &$CodeQLPath database cleanup $Database --mode=brutal | ForEach-Object { Write-Host $_ }
    $Results = &$CodeQLPath database run-queries $Database $FullQuery --ram=8192 --tuple-counting --verbosity=progress+++ 2>&1 |
        ForEach-Object {
            Write-Host $_
            if ($_ -match 'Evaluation done (\([^\)]+\))') {
                Write-Host "Match: $($Matches[1])"
                @{
                    Database = $Database;
                    Library = $Library;
                    EvaluationTime = $Matches[1];
                }
            }
        }

    return $Results
}

$Databases = @(
#    'C:\src\Snapshots\Perf\Attnam_ivan_4bdf1a9'
    'C:\src\Snapshots\Perf\awslabs_s2n_2d4ca12'
#    'C:\src\Snapshots\Perf\vrastil_FastSim_602e70b'
#'C:\src\Snapshots\Perf\ElektraInitiative_libelektra_700eefa',
#'C:\src\Snapshots\Perf\radareorg_cutter_63ae6e6',
#'C:\src\Snapshots\Perf\radareorg_radare2_0a86b4c',
#'C:\src\Snapshots\Perf\SinaMostafanejad_OpenRDM_3c73dec',
#'C:\src\Snapshots\Perf\zcoinofficial_zcoin_1392762'
)

$env:SEMMLE_SYNCHRONOUS_LOGGING='true'
$Results = @()
foreach ($Database in $Databases) {
    foreach ($Library in @('C:\Src\ql_base\cpp\ql\src', 'C:\Src\ql_4\cpp\ql\src')) {
        $Results += RunQuery -Query 'Likely Bugs\RedundantNullCheckSimple.ql' -Database $Database -Library $Library
    }
}

return $Results
