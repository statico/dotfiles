<script language="javascript" type="text/javascript">
//<!--
{literal}
$('account').observe("change", function(event) {should_get_account_data(
'first', 'acct' );});
$('desc').observe("change", function(event) {should_get_account_data(
'first', 'name' );});
{/literal}
{if $autocomplete}
{literal}
document.observe("dom:loaded", function(event)
{auto_complete_customers('first', 'acct');});
document.observe("dom:loaded", function(event)
{auto_complete_customers('first', 'name');});
{/literal}
{/if}
//-->
</script>
