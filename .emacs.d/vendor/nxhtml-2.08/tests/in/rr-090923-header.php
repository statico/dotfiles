<?php
  require_once("utils.php");
?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">

  <?php if(!processLanguageURL())
    {
	    if ($_SESSION["CurrentLanguage"]==0){
		    $_SESSION["CurrentLanguage"]=$Default_Lang;
	    }
    }
  ?>

  <head>

    <title><?php echo getMessage("website.title");?></title>
    
    <meta http-equiv="Content-Type" content="text/html;charset=utf-8" />

    <?php
      if (AUTO_REFRESH_DELAY>0 && !adminMode())
      echo '<meta http-equiv="refresh" content="'.AUTO_REFRESH_DELAY.'"/>';
    ?>


    <link type="text/css" href="jscalendar-1.0/calendar-win2k-1.css" rel="stylesheet" title="Style Sheet"/> <!-- indicate local copy -->
    <script type="text/javascript" src="jscalendar-1.0/calendar.js"></script>
    <script type="text/javascript" src="jscalendar-1.0/lang/calendar-en.js"></script>
    <script type="text/javascript" src="jscalendar-1.0/calendar-setup.js"></script>


    <link type="text/css" href="stylesheet.css" rel="stylesheet" title="Style Sheet" />
    <link type="text/css" href="print.css" rel="stylesheet" media="print"/>
   
    <!--[if lt IE 7]>
	<link type="text/css" href="ie.css" rel="stylesheet" title="Style Sheet"/>
	<link type="text/css" href="site-ie.css" rel="stylesheet" title="Style Sheet"/>
	<![endif]-->

    
    <link type="text/css" href="/custom/site.css" rel="stylesheet" title="Style Sheet"/> <!-- project custom -->
    <style type="text/css"><?php echo htmlentities(getMessage("header.custom.css"));?> </style>


<?php  undercon();
echoHeaderContent("keywords",curPageName());
echoHeaderContent("description",curPageName());
echoHeaderContent("verify-v1",curPageName());
?>
    
    <link rel="SHORTCUT ICON" href="favicon.ico"/>

    <META NAME="AUTHOR" CONTENT="Richard G. Riley"/>
      <meta name="copyright" content="Copyright Richard G. Riley 2009" />


    
  </head>
  
  <body  id="body">

    <div id="wcbg">
      
      <div id="wc"  class="clearfix">

    <?php
      checkWebLicense();
    ?>

	<?php
	  include("navigation.php");
	?>

	<div id="wb">
	  
	  <?php 
	    if(isDemo()){
	    echo '<div class="demomode">';
	    echoMessage("admin.demo");
	    echo "</div>";
	    }
	    if(adminMode()){
	    echo '<div id="admincontrolcontainer">';
	    createNavigationLinks("navlinks",1);
	    echo '<div class="clear"></div>';
	    echo '</div>';
	    }	  
	  ?>
	  
	  <div id="centercontainer">
	    <?php
	      if(messageMode()){
	      echo '<div class="editwebtitle">';
	      echoMessage("website.title");
	      echo '</div>';
	      }
	    ?>

