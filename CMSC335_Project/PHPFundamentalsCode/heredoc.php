<!doctype html>
<html>
    <head> 
        <meta charset="UTF-8" /> 
		<title>PHP Example</title>	
	</head>

	<body>
		<?php
			$name = "Bart Simpson";
	
			$information = <<<DATA
				<form action="heredoc.php" method="post">
				<p>
				<em>Character:      </em><input type="text" value="$name" />
				</p>
				</form>
DATA;
			echo $information;	
		?>
	</body>
</html>