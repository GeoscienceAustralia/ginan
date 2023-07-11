def match_patterns(patterns, value):
    for pattern in patterns:
        if is_valid_pattern(pattern):
            if matches_pattern(pattern, value):
                return True
        else:
            raise ValueError(f"{pattern} invalid")
    return False


def is_valid_pattern(pattern):
    if pattern.count("*") > 1:
        return False
    return True


def matches_pattern(pattern, value):
    if "?" in pattern:
        return matches_question_mark_pattern(pattern, value)
    if "*" in pattern:
        return matches_asterisk_pattern(pattern, value)
    return pattern == value


def matches_question_mark_pattern(pattern, value):
    if len(pattern) != len(value):
        return False

    for p, v in zip(pattern, value):
        if p != "?" and p != v:
            return False

    return True


def matches_asterisk_pattern(pattern, value):
    """
    @todo right now it will work only with one asterisk.
    :param pattern:
    :param value:
    :return:
    """
    pattern_parts = pattern.split("*")
    if len(pattern_parts) == 1 and value == pattern:
        return True
    if pattern_parts[0] == "":
        return value.endswith(pattern_parts[-1])
    if pattern_parts[-1] == "":
        return value.startswith(pattern_parts[0])
    return value.startswith(pattern_parts[0]) and value.endswith(pattern_parts[-1])


def generate_list(pattern, list):
    if pattern:
        return [value for value in list if match_patterns(pattern, value)]
    else:
        return [""]
