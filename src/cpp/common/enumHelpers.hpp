#pragma once

#include <optional>
#include <stdexcept>
#include <string>
#include "3rdparty/magic_enum.hpp"

/**
 * Helper functions to simplify magic_enum usage and provide
 * drop-in replacements for BETTER_ENUM functionality
 */

// Enum to string conversion (replaces ._to_string())
template <typename E>
inline std::string enum_to_string(E value)
{
    return std::string(magic_enum::enum_name(value));
}

// String to enum conversion with default fallback (replaces ::_from_string())
template <typename E>
inline E string_to_enum(const std::string& str, E default_value = E{})
{
    return magic_enum::enum_cast<E>(str).value_or(default_value);
}

// String to enum with optional return
template <typename E>
inline std::optional<E> string_to_enum_opt(const std::string& str)
{
    return magic_enum::enum_cast<E>(str);
}

// Case-insensitive string to enum (replaces ::_from_string_nocase())
template <typename E>
inline E string_to_enum_nocase(const std::string& str, E default_value = E{})
{
    auto result = magic_enum::enum_cast<E>(str, magic_enum::case_insensitive);
    return result.value_or(default_value);
}

// Case-insensitive string to enum with exception (for compatibility)
template <typename E>
inline E string_to_enum_nocase_throw(const char* str)
{
    auto result = magic_enum::enum_cast<E>(str, magic_enum::case_insensitive);
    if (!result.has_value())
    {
        throw std::runtime_error(std::string("Invalid enum value: ") + str);
    }
    return result.value();
}

template <typename E>
inline E string_to_enum_nocase_throw(std::string str)
{
    return string_to_enum_nocase_throw<E>(str.c_str());
}

// Integer to enum conversion (replaces ::_from_integral())
template <typename E>
inline E int_to_enum(int value)
{
    return static_cast<E>(value);
}

// Integer to enum with validation
template <typename E>
inline std::optional<E> int_to_enum_opt(int value)
{
    return magic_enum::enum_cast<E>(value);
}

// Integer to enum with default fallback
template <typename E>
inline E int_to_enum_safe(int value, E default_value = E{})
{
    return magic_enum::enum_cast<E>(value).value_or(default_value);
}

// Enum to int helper (explicit conversion)
template <typename E>
inline int enum_to_int(E value)
{
    return static_cast<int>(value);
}

// Enum arithmetic helper - add offset to enum
template <typename E>
inline E enum_add(E base, int offset)
{
    return static_cast<E>(static_cast<int>(base) + offset);
}

// Enum arithmetic helper - subtract to get offset
template <typename E>
inline int enum_diff(E a, E b)
{
    return static_cast<int>(a) - static_cast<int>(b);
}

// Check if string is valid enum value (replaces ::_is_valid())
template <typename E>
inline bool is_valid_enum_string(const std::string& str)
{
    return magic_enum::enum_cast<E>(str).has_value();
}

// Check if integer is valid enum value (replaces ::_is_valid())
template <typename E>
inline bool is_valid_enum_int(int value)
{
    return magic_enum::enum_cast<E>(value).has_value();
}

// Get enum count (replaces ::_size())
template <typename E>
inline constexpr std::size_t enum_count()
{
    return magic_enum::enum_count<E>();
}

// Get enum values array (replaces ::_values())
template <typename E>
inline constexpr auto enum_values()
{
    return magic_enum::enum_values<E>();
}

// Get enum names array
template <typename E>
inline constexpr auto enum_names()
{
    return magic_enum::enum_names<E>();
}

// Stream output operator for enums (enables logging/printing)
template <typename E>
inline typename std::enable_if<std::is_enum<E>::value, std::ostream&>::type
operator<<(std::ostream& os, E value)
{
    return os << enum_to_string(value);
}
