{include file="library/header.tpl"}

{* comment *}

{literal}
<script language="JavaScript" type="text/javascript">
function validate_form(f){
    allElements = f.getInputs('text');
    var get_time = /^times\[(.+)\]$/;
    var get_date = /^dates\[(.+)\]$/;
    for(var index=0; index < allElements.length; ++index){
        var item = allElements[index];
        if(get_time.test(item.name)){
            if($F(item)){
                var key = get_time.exec(item.name)
                var sibling_date = 'dates[' + key[1] + ']';
                if(! $F(sibling_date)){
                    alert("Date is required if entering a manual time!");
                    $(sibling_date).addClassName('problem');
                    return false;
                }
            }
                             
        }
    }
    else if(get_date.test(item.name)){
        if($F(item)){
            var key = get_date.exec(item.name)
            var sibling_time = 'times[' + key[1] + ']';
            if(! $F(sibling_time)){
                alert("Time is required if entering a manual date!");
                $(sibling_time).addClassName('problem');
                return false;
            }
        }
    }
    return true;
}

function validate_date( el )
{
    var date = /^\d\d\d\d-\d\d-\d\d$/;
    if ( !date.test(el.value) ) {
        alert('The date you have entered is not properly formatted (yyyy-mm-dd).  Please re-enter it.');
        el.value='';
        el.focus();
        el.className='problem';
        return false;
    }
    el.className='';
    return true;
}

function validate_time( el )
{
    var time = /^\d\d:\d\d$/;
    if ( !time.test(el.value) ) {
        alert('The time you have entered is not properly formatted (hh:mm).  Please re-enter it.');
        el.value='';
        el.focus();
        el.className = 'problem';
        return false;
    }
    el.className = '';
    return true;
}
</script>

<style type="text/css">
  foo: {
         font-weight: bold;
         color: #F00;
         }
</style>
{/literal}

{if $containers|@count }

<form name="test" method=post onsubmit="return validate_form(this);">
<input type="hidden" name="date" value="{$smarty.request.date}">
<input type="hidden" name="page_name" value="{$page_name}" />

<table frame="void" bordercolor="#000000" rules="cols" class="data" style="float:center;">
<thead>
<tr class="sortHeader">
    {if $goahead}
    <th onclick="sortTable(this)">{t}Date{/t}<br /><span class="ex">YYYY-MM-DD</span></th>
    {/if}
    {if $goahead}
    <th onclick="sortTable(this)">{t}Time{/t}<br /><span class="ex">HH:MM</span></th>
    {/if}

    <th onclick="sortTable(this)">{t}Quantity{/t}</th>
</tr>
</thead>

{foreach from=$widgets item="widget"}
<tbody class="{cycle values=",highlight}" ondblclick="return insertHeader(this);">
    {if $widget->date != $today}
        {assign var=newday value=true}
        {assign var=today value=$widget->date}
    {else}
        {assign var=newday value=false}
    {/if}

<tr class="{if $newday}group{/if}">
    {if $goahead}
    {if $widget->activity_allowed}
    <td class="ctext"><input type="text" value="" onchange="validate_date(this, this.form );" name="dates[{$widget->widget_id_for_web}]" id="dates[{$widget->widget_id_for_web}]" size=10 maxlength=10></td>
    {else}
    <td></td>
    {/if}
    {/if}

    {if $goahead}
    {if $widget->activity_allowed}
    <td class="ctext"><input type="text" value="" onchange="validate_time(this, this.form);" name="times[{$widget->widget_id_for_web}]" id="times[{$widget->widget_id_for_web}]"size=5 maxlength=5></td>
    {else}
    <td></td>
    {/if}
    {/if}

    <td>{$widget->quantity}</td>
</tr>
</tbody>
{/foreach}
</table>

<hr />
    {if $goahead}
    <br />
    <b>{t}Note{/t}:</b> {t}Changes may not occur immediately.{/t}
    <br />
    <input type=submit value="Update Widgets">
    {/if}

    </form>
{else}
    {t}There are no widgets on this day{/t}
{/if}

{literal}
<hr/>test

<p>Paragraph</p>
{/literal}

{include file="library/footer.tpl"}
