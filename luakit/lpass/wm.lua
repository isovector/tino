local select = require("select_wm")
local lousy = require("lousy")
local ui = ipc_channel("lpass.wm")
local filter = lousy.util.table.filter_array

-- browserpass form search logic: https://github.com/dannyvankooten/browserpass/blob/master/chrome/inject.js

local FORM_MARKERS = {
    "login",
    "log-in",
    "log_in",
    "signin",
    "sign-in",
    "sign_in"
}

local USERNAME_FIELDS = {
    selectors = {
        "input[name*=user]",
        "input[name*=login]",
        "input[name*=email]",
        "input[id*=user]",
        "input[id*=login]",
        "input[id*=email]",
        "input[class*=user]",
        "input[class*=login]",
        "input[class*=email]",
        "input[type=email]",
        "input[type=text]",
        "input[type=tel]"
    },
    types = {"email", "text", "tel"}
}

local PASSWORD_FIELDS = {
    selectors = {"input[type=password]"}
}

local INPUT_FIELDS = {
    selectors = lousy.util.table.join(PASSWORD_FIELDS.selectors, USERNAME_FIELDS.selectors)
}

local SUBMIT_FIELDS = {
    selectors = {
        "[type=submit]",
        "button[name*=login]",
        "button[name*=log-in]",
        "button[name*=log_in]",
        "button[name*=signin]",
        "button[name*=sign-in]",
        "button[name*=sign_in]",
        "button[id*=login]",
        "button[id*=log-in]",
        "button[id*=log_in]",
        "button[id*=signin]",
        "button[id*=sign-in]",
        "button[id*=sign_in]",
        "button[class*=login]",
        "button[class*=log-in]",
        "button[class*=log_in]",
        "button[class*=signin]",
        "button[class*=sign-in]",
        "button[class*=sign_in]",
        "input[type=button][name*=login]",
        "input[type=button][name*=log-in]",
        "input[type=button][name*=log_in]",
        "input[type=button][name*=signin]",
        "input[type=button][name*=sign-in]",
        "input[type=button][name*=sign_in]",
        "input[type=button][id*=login]",
        "input[type=button][id*=log-in]",
        "input[type=button][id*=log_in]",
        "input[type=button][id*=signin]",
        "input[type=button][id*=sign-in]",
        "input[type=button][id*=sign_in]",
        "input[type=button][class*=login]",
        "input[type=button][class*=log-in]",
        "input[type=button][class*=log_in]",
        "input[type=button][class*=signin]",
        "input[type=button][class*=sign-in]",
        "input[type=button][class*=sign_in]"
    }
}

local function elem_meta(page)
    return page:wrap_js([=[
        return {
            "form_matches": !form || elem.form == form,
            "offset": {
                "width": elem.offsetWidth,
                "height": elem.offsetHeight,
            },
            "bounding_client_rect": elem.getBoundingClientRect(),
            "window_inner": {
                "width": window.innerWidth,
                "height": window.innerHeight,
            },
        };
    ]=], {"elem", "form"})
end

local function elem_focus(page)
    return page:wrap_js([=[
        var eventNames = ["click", "focus"];
        eventNames.forEach(function(eventName) {
          elem.dispatchEvent(new Event(eventName, { bubbles: true }));
        });
    ]=], {"elem"})
end

local function elem_unfocus(page)
    return page:wrap_js([=[
        elem.setAttribute("value", value);
        elem.value = value;
        var eventNames = [
            "keypress",
            "keydown",
            "keyup",
            "input",
            "blur",
            "change"
        ];
        eventNames.forEach(function(eventName) {
            elem.dispatchEvent(new Event(eventName, { bubbles: true }));
        });
    ]=], {"elem"})
end

local function find(page, root, selectors)
    if type(selectors) == "string" then
        selectors = { selectors = selectors }
    end

    local meta = elem_meta(page);
    local matches = {}

    for _, sel in ipairs(selectors.selectors) do
        for _, element in ipairs(root:query(sel)) do
            local matches_ty = true
            if selectors.types ~= nil then
                matches_ty = false
                for _, ty in ipairs(selectors.types) do
                    matches_ty = ty == element.type
                    if matches_ty then
                        break
                    end
                end
            end
            if matches_ty then
                local data = meta(element)

                if data.offset.width < 30 or data.offset.height < 10 then
                    matches_ty = false
                end

                local style = element.style
                if style.visibility == "hidden" or style.display == "none" then
                    matches_ty = false
                end

                if data.bounding_client_rect.x + data.bounding_client_rect.width < 0 or
                    data.bounding_client_rect.y + data.bounding_client_rect.height < 0 or
                    data.bounding_client_rect.x > data.window_inner.width or
                    data.bounding_client_rect.y > data.window_inner.height then
                    matches_ty = false
                end
            end

            if matches_ty then
                table.insert(matches, element)
            end
        end
    end

    return matches
end

ui:add_signal("fill", function (_, page, data)
    page:eval_js("console.log('what up')")
    local root = page.document.body
    local focus = elem_focus(page)
    local unfocus = elem_unfocus(page)
    local username_field = find(page, root, USERNAME_FIELDS)
    if #username_field > 0 then
        focus(username_field[1])
        username_field = find(page, root, USERNAME_FIELDS)
    end
    if #username_field > 0 then
        if data.username ~= nil then
            username_field[1].value = data.username
            unfocus(username_field[1])
        end
    end

    local password_field = find(page, root, PASSWORD_FIELDS)
    if #password_field > 0 then
        focus(password_field[1])
        password_field = find(page, root, PASSWORD_FIELDS)
    end
    if #password_field > 0 then
        if data.password ~= nil then
            password_field[1].value = data.password
            unfocus(password_field[1])
        end
    end

    if #password_field > 1 then
        msg.warn("still more to fill, otp code maybe?")
        focus(password_field[2])
    elseif #password_field == 1 then
        focus(password_field[1])
    elseif #username_field > 0 then
        focus(username_field[1])
    end

    local submit = find(page, root, SUBMIT_FIELDS)
    if #submit > 0 then
        if data.submit then
            focus(submit[1])
        end
    end
end)

ui:add_signal("fill_otp", function (_, page, data)
    local fn = page:wrap_js([=[
        document.activeElement.value = value;
    ]=], {"value"})
    fn(data.otp)
end)

-- vim: et:sw=4:ts=8:sts=4:tw=80
