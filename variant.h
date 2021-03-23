#include <cstring>
#include <functional>

inline constexpr size_t variant_npos = -1;

template <class T> struct in_place_type_t { explicit in_place_type_t() = default; };

template <class T> inline constexpr in_place_type_t<T> in_place_type{};

template <size_t I> struct in_place_index_t { explicit in_place_index_t() = default; };

template <size_t I> inline constexpr in_place_index_t<I> in_place_index{};
template <typename T> struct is_in_place_t_impl : std::false_type {};

template <size_t N> struct is_in_place_t_impl<in_place_index_t<N>> : std::true_type {};

template <typename T, typename... Ts> struct counter : std::integral_constant<size_t, 0> {};

template <typename T, typename F, typename... Fs>
struct counter<T, F, Fs...> : std::integral_constant<size_t, counter<T, Fs...>::value + std::is_same_v<T, F>> {};

template <typename T, size_t Num, typename... Types> struct index_by_type_impl;

template <typename T, size_t Num, typename F, typename... Rest> struct index_by_type_impl<T, Num, F, Rest...> {
  static constexpr size_t index = std::is_same_v<T, F> ? Num : index_by_type_impl<T, Num + 1, Rest...>::index;
};

template <typename T, size_t Num> struct index_by_type_impl<T, Num> { static constexpr size_t index = variant_npos; };

template <typename T, typename... Types>
inline constexpr size_t index_by_type_v = index_by_type_impl<T, 0, Types...>::index;

template <size_t N, typename Type0, typename... Types> struct type_by_index_impl {
  using value_type = typename type_by_index_impl<N - 1, Types...>::value_type;
};

template <typename Type0, typename... Types> struct type_by_index_impl<0, Type0, Types...> {
  using value_type = Type0;
};

template <size_t N, typename... Types> using type_by_index_t = typename type_by_index_impl<N, Types...>::value_type;

template <typename... Types> struct variant;

template <typename T_j> struct array_init { T_j x[1]; };
template <typename T, typename T_j, bool is_T_j_bool = std::is_same_v<std::remove_cv_t<T_j>, bool>, typename = void>
struct img_func {
  void F();
};
template <typename T, typename T_j>
struct img_func<T, T_j, false, std::void_t<decltype(array_init<T_j>{std::declval<T>()})>> {
  static T_j F(T_j);
};
template <typename T, typename T_j>
struct img_func<T, T_j, true, std::enable_if_t<std::is_same_v<std::remove_cv_t<std::remove_reference_t<T>>, bool>>> {
  static T_j F(T_j);
};
template <typename T, typename Variant> struct img_funcs;
template <typename T, typename... T_j> struct img_funcs<T, variant<T_j...>> : img_func<T, T_j>... {
  using img_func<T, T_j>::F...;
};
template <typename T, typename Variant> using best_choice = decltype(img_funcs<T, Variant>::F(std::declval<T>()));

template <typename V> struct variant_size;
template <typename... Types>
struct variant_size<variant<Types...>> : std::integral_constant<size_t, sizeof...(Types)> {};
template <typename V> struct variant_size<const V> : variant_size<V> {};
template <typename V> struct variant_size<volatile V> : variant_size<V> {};
template <typename V> struct variant_size<const volatile V> : variant_size<V> {};
template <typename V> inline constexpr size_t variant_size_v = variant_size<V>::value;

template <std::size_t I, typename T> struct variant_alternative;
template <std::size_t I, typename... Types> struct variant_alternative<I, variant<Types...>> {
  using type = type_by_index_t<I, Types...>;
};
template <std::size_t I, typename T> struct variant_alternative<I, const T> : variant_alternative<I, T> {
  using type = std::add_const_t<typename variant_alternative<I, T>::type>;
};
template <std::size_t I, typename T> struct variant_alternative<I, volatile T> : variant_alternative<I, T> {
  using type = std::add_volatile_t<typename variant_alternative<I, T>::type>;
};
template <std::size_t I, typename T> struct variant_alternative<I, const volatile T> : variant_alternative<I, T> {
  using type = std::add_cv_t<typename variant_alternative<I, T>::type>;
};
template <size_t I, typename T> using variant_alternative_t = typename variant_alternative<I, T>::type;

namespace util {

template <typename T> static void destructor_call(void *obj) { static_cast<T *>(obj)->~T(); }

template <typename T> static void copy_ctor_call(const void *src_stg, void *dst_stg) {
  const T &other = *static_cast<const T *>(src_stg);
  new (dst_stg) T(other);
}

template <typename T> static void move_ctor_call(void *src_stg, void *dst_stg) {
  new (dst_stg) T(std::move(*static_cast<T *>(src_stg)));
}

template <typename T> static void move_assign_call(void *src_stg, void *dst_stg) {
  T &this_obj = *static_cast<T *>(dst_stg);
  T &other_obj = *static_cast<T *>(src_stg);
  this_obj = std::move(other_obj);
}

template <typename T> static void swap_call(void *src_stg, void *dst_stg) {
  T &this_obj = *static_cast<T *>(dst_stg);
  T &other_obj = *static_cast<T *>(src_stg);
  using std::swap;
  swap(this_obj, other_obj);
}

template <typename T> static void copy_assign_call(void const *src_stg, void *dst_stg) {
  T &this_obj = *static_cast<T *>(dst_stg);
  const T &other_obj = *static_cast<const T *>(src_stg);
  this_obj = other_obj;
}
} // namespace util

template <typename... Types> struct traits {
  static constexpr bool is_default_constructible = (std::is_default_constructible_v<type_by_index_t<0, Types...>>);
  static constexpr bool is_copy_constructible = (std::is_copy_constructible_v<Types> && ...);
  static constexpr bool is_move_constructible = (std::is_move_constructible_v<Types> && ...);
  static constexpr bool is_copy_assignable = is_copy_constructible && (std::is_copy_assignable_v<Types> && ...);
  static constexpr bool is_move_assignable = is_move_constructible && (std::is_move_assignable_v<Types> && ...);

  static constexpr bool is_trivially_destructible = (std::is_trivially_destructible_v<Types> && ...);
  static constexpr bool is_trivially_copy_constructible = (std::is_trivially_copy_constructible_v<Types> && ...);
  static constexpr bool is_trivially_move_constructible = (std::is_trivially_move_constructible_v<Types> && ...);
  static constexpr bool is_trivially_copy_assignable = is_trivially_copy_constructible && is_trivially_destructible &&
                                                       (std::is_trivially_copy_assignable_v<Types> && ...);
  static constexpr bool is_trivially_move_assignable = is_trivially_move_constructible && is_trivially_destructible &&
                                                       (std::is_trivially_move_assignable_v<Types> && ...);
};

template <typename... Types> union trivial_union_storage {};

template <typename Type0, typename... OtherTypes> union trivial_union_storage<Type0, OtherTypes...> {
  constexpr trivial_union_storage() : others() {}

  template <typename... Args>
  constexpr trivial_union_storage(in_place_index_t<0>, Args &&...args) : alternative(std::forward<Args>(args)...) {}

  template <size_t Num, typename... Args>
  constexpr trivial_union_storage(in_place_index_t<Num>, Args &&...args)
      : others(in_place_index<Num - 1>, std::forward<Args>(args)...) {}

  constexpr Type0 &get_ref(in_place_index_t<0>) { return alternative; }
  constexpr Type0 const &get_ref(in_place_index_t<0>) const { return alternative; }

  template <size_t Num> constexpr decltype(auto) get_ref(in_place_index_t<Num>) {
    return others.get_ref(in_place_index<Num - 1>);
  }

  template <size_t Num> constexpr decltype(auto) get_ref(in_place_index_t<Num>) const {
    return others.get_ref(in_place_index<Num - 1>);
  }

  constexpr Type0 *get_ptr(in_place_index_t<0>) { return std::addressof(alternative); }
  constexpr Type0 const *get_ptr(in_place_index_t<0>) const { return std::addressof(alternative); }

  template <size_t Num> constexpr decltype(auto) get_ptr(in_place_index_t<Num>) {
    return others.get_ptr(in_place_index<Num - 1>);
  }
  template <size_t Num> constexpr decltype(auto) get_ptr(in_place_index_t<Num>) const {
    return others.get_ptr(in_place_index<Num - 1>);
  }

  Type0 alternative;
  trivial_union_storage<OtherTypes...> others;
};

template <typename... Types> union non_trivial_union_storage {};

template <typename Type0, typename... OtherTypes> union non_trivial_union_storage<Type0, OtherTypes...> {
  constexpr non_trivial_union_storage() : others() {}

  template <typename... Args>
  constexpr non_trivial_union_storage(in_place_index_t<0>, Args &&...args) : alternative(std::forward<Args>(args)...) {}

  template <size_t Num, typename... Args>
  constexpr non_trivial_union_storage(in_place_index_t<Num>, Args &&...args)
      : others(in_place_index<Num - 1>, std::forward<Args>(args)...) {}
  ~non_trivial_union_storage() {}

  constexpr Type0 &get_ref(in_place_index_t<0>) { return alternative; }
  constexpr Type0 const &get_ref(in_place_index_t<0>) const { return alternative; }

  template <size_t Num> constexpr decltype(auto) get_ref(in_place_index_t<Num>) {
    return others.get_ref(in_place_index<Num - 1>);
  }
  template <size_t Num> constexpr decltype(auto) get_ref(in_place_index_t<Num>) const {
    return others.get_ref(in_place_index<Num - 1>);
  }

  constexpr Type0 *get_ptr(in_place_index_t<0>) { return std::addressof(alternative); }
  constexpr Type0 const *get_ptr(in_place_index_t<0>) const { return std::addressof(alternative); }

  template <size_t Num> constexpr decltype(auto) get_ptr(in_place_index_t<Num>) {
    return others.get_ptr(in_place_index<Num - 1>);
  }
  template <size_t Num> constexpr decltype(auto) get_ptr(in_place_index_t<Num>) const {
    return others.get_ptr(in_place_index<Num - 1>);
  }

  Type0 alternative;
  non_trivial_union_storage<OtherTypes...> others;
};

template <bool is_trivially_destructible, typename... Types> struct destruction_base {
protected:
  constexpr destruction_base() : current_index(variant_npos){};

  template <size_t Num, typename... Args>
  constexpr destruction_base(in_place_index_t<Num> num, Args &&...args)
      : current_index(Num), storage(num, std::forward<Args>(args)...) {}

  ~destruction_base() { destruct(); }

  constexpr void destruct() noexcept {
    if (current_index != variant_npos) {
      destructor_funcs[current_index](static_cast<void *>(&storage));
    }
  }

  size_t current_index = variant_npos;
  non_trivial_union_storage<Types...> storage;

private:
  using destructor_func_t = void (*)(void *obj);
  constexpr static destructor_func_t destructor_funcs[sizeof...(Types)] = {&util::destructor_call<Types>...};
};

template <typename... Types> struct destruction_base<true, Types...> {
protected:
  constexpr destruction_base() : current_index(variant_npos){};

  template <size_t Num, typename... Args>
  constexpr destruction_base(in_place_index_t<Num> num, Args &&...args)
      : current_index(Num), storage(num, std::forward<Args>(args)...) {}

  constexpr void destruct() noexcept {}

  size_t current_index = variant_npos;
  trivial_union_storage<Types...> storage;
};

template <bool is_move_constructible, bool is_trivially_move_constructible, typename... Types>
struct move_constructible_base : destruction_base<traits<Types...>::is_trivially_destructible, Types...> {
protected:
  using base = destruction_base<traits<Types...>::is_trivially_destructible, Types...>;
  using base::base;

  constexpr move_constructible_base(move_constructible_base &&other) = delete;

  move_constructible_base(move_constructible_base const &) = default;
  move_constructible_base &operator=(const move_constructible_base &) = default;
  move_constructible_base &operator=(move_constructible_base &&) = default;
};

template <typename... Types>
struct move_constructible_base<true, true, Types...>
    : destruction_base<traits<Types...>::is_trivially_destructible, Types...> {
protected:
  using base = destruction_base<traits<Types...>::is_trivially_destructible, Types...>;
  using base::base;

  // move obj from other storage to the current storage
  constexpr void move_storage(move_constructible_base &&other) { new (this) move_constructible_base(std::move(other)); }
};

template <typename... Types>
struct move_constructible_base<true, false, Types...>
    : destruction_base<traits<Types...>::is_trivially_destructible, Types...> {
protected:
  using base = destruction_base<traits<Types...>::is_trivially_destructible, Types...>;
  using base::base;

  constexpr move_constructible_base(move_constructible_base &&other) noexcept(
      (std::is_nothrow_move_constructible_v<Types> && ...)) {
    if (other.current_index != variant_npos) {
      move_storage(std::move(other));
    }
  };

  move_constructible_base(move_constructible_base const &) = default;
  move_constructible_base &operator=(const move_constructible_base &) = default;
  move_constructible_base &operator=(move_constructible_base &&) = default;

  // move obj from other storage to the current storage
  constexpr void move_storage(move_constructible_base &&other) {
    move_ctor_funcs[other.current_index](&other.storage, &this->storage);
    this->current_index = other.current_index;
  }

private:
  using move_ctor_func_t = void (*)(void *source_stg, void *this_stg);
  constexpr static move_ctor_func_t move_ctor_funcs[sizeof...(Types)] = {&util::move_ctor_call<Types>...};
};

template <bool is_copy_constructible, bool is_trivially_copy_constructible, typename... Types>
struct copy_constructible_base : move_constructible_base<traits<Types...>::is_move_constructible,
                                                         traits<Types...>::is_trivially_move_constructible, Types...> {
protected:
  using base = move_constructible_base<traits<Types...>::is_move_constructible,
                                       traits<Types...>::is_trivially_move_constructible, Types...>;
  using base::base;

  constexpr copy_constructible_base(copy_constructible_base const &other) = delete;

  copy_constructible_base(copy_constructible_base &&) = default;
  copy_constructible_base &operator=(const copy_constructible_base &) = default;
  copy_constructible_base &operator=(copy_constructible_base &&) = default;
};

template <typename... Types>
struct copy_constructible_base<true, true, Types...>
    : move_constructible_base<traits<Types...>::is_move_constructible,
                              traits<Types...>::is_trivially_move_constructible, Types...> {
protected:
  using base = move_constructible_base<traits<Types...>::is_move_constructible,
                                       traits<Types...>::is_trivially_move_constructible, Types...>;
  using base::base;
};

template <typename... Types>
struct copy_constructible_base<true, false, Types...>
    : move_constructible_base<traits<Types...>::is_move_constructible,
                              traits<Types...>::is_trivially_move_constructible, Types...> {
protected:
  using base = move_constructible_base<traits<Types...>::is_move_constructible,
                                       traits<Types...>::is_trivially_move_constructible, Types...>;
  using base::base;

  constexpr copy_constructible_base(const copy_constructible_base &other) noexcept(
      (std::is_nothrow_copy_constructible_v<Types> && ...))
      : base() {
    if (other.current_index != variant_npos) {
      copy_ctor_funcs[other.current_index](&other.storage, &(this->storage));
    }
    this->current_index = other.current_index;
  };

  copy_constructible_base(copy_constructible_base &&) = default;
  copy_constructible_base &operator=(const copy_constructible_base &) = default;
  copy_constructible_base &operator=(copy_constructible_base &&) = default;

private:
  using copy_ctor_func_t = void (*)(const void *source_stg, void *this_stg);
  constexpr static copy_ctor_func_t copy_ctor_funcs[sizeof...(Types)] = {&util::copy_ctor_call<Types>...};
};

template <bool is_move_assignable, bool is_trivially_move_assignable, typename... Types>
struct move_assignable_base : copy_constructible_base<traits<Types...>::is_copy_constructible,
                                                      traits<Types...>::is_trivially_copy_constructible, Types...> {
protected:
  using base = copy_constructible_base<traits<Types...>::is_copy_constructible,
                                       traits<Types...>::is_trivially_copy_constructible, Types...>;
  using base::base;

  constexpr move_assignable_base &operator=(move_assignable_base &&other) = delete;

  move_assignable_base(move_assignable_base &&) = default;
  move_assignable_base(move_assignable_base const &) = default;
  move_assignable_base &operator=(const move_assignable_base &) = default;
};

template <typename... Types>
struct move_assignable_base<true, true, Types...>
    : copy_constructible_base<traits<Types...>::is_copy_constructible,
                              traits<Types...>::is_trivially_copy_constructible, Types...> {
protected:
  using base = copy_constructible_base<traits<Types...>::is_copy_constructible,
                                       traits<Types...>::is_trivially_copy_constructible, Types...>;
  using base::base;
};

template <typename... Types>
struct move_assignable_base<true, false, Types...>
    : copy_constructible_base<traits<Types...>::is_copy_constructible,
                              traits<Types...>::is_trivially_copy_constructible, Types...> {
protected:
  using base = copy_constructible_base<traits<Types...>::is_copy_constructible,
                                       traits<Types...>::is_trivially_copy_constructible, Types...>;
  using base::base;

  constexpr move_assignable_base &operator=(move_assignable_base &&other) noexcept(
      ((std::is_nothrow_move_constructible_v<Types> && std::is_nothrow_move_assignable_v<Types>)&&...)) {
    if (other.current_index == this->current_index) {
      if (other.current_index == variant_npos) {
        return *this;
      } else {
        move_assign_funcs[other.current_index](&other.storage, &(this->storage));
      }
    } else if (other.current_index == variant_npos) {
      this->destruct();
      this->current_index = variant_npos;
    } else {
      this->destruct();
      this->current_index = variant_npos;
      this->move_storage(std::move(other));
    }
    return *this;
  }

  move_assignable_base(move_assignable_base &&) = default;
  move_assignable_base(move_assignable_base const &) = default;
  move_assignable_base &operator=(const move_assignable_base &) = default;

private:
  using move_assign_func_t = void (*)(void *source_stg, void *this_stg);
  constexpr static move_assign_func_t move_assign_funcs[sizeof...(Types)] = {&util::move_assign_call<Types>...};
};

template <bool is_copy_assignable, bool is_trivially_copy_assignable, typename... Types>
struct copy_assignable_base : move_assignable_base<traits<Types...>::is_move_assignable,
                                                   traits<Types...>::is_trivially_move_assignable, Types...> {
protected:
  using base = move_assignable_base<traits<Types...>::is_move_assignable,
                                    traits<Types...>::is_trivially_move_assignable, Types...>;
  using base::base;

  constexpr copy_assignable_base &operator=(copy_assignable_base const &) = delete;

  copy_assignable_base(copy_assignable_base &&) = default;
  copy_assignable_base(copy_assignable_base const &) = default;
  copy_assignable_base &operator=(copy_assignable_base &&) = default;
};

template <typename... Types>
struct copy_assignable_base<true, true, Types...>
    : move_assignable_base<traits<Types...>::is_move_assignable, traits<Types...>::is_trivially_move_assignable,
                           Types...> {
protected:
  using base = move_assignable_base<traits<Types...>::is_move_assignable,
                                    traits<Types...>::is_trivially_move_assignable, Types...>;
  using base::base;
};

template <typename... Types>
struct copy_assignable_base<true, false, Types...>
    : move_assignable_base<traits<Types...>::is_move_assignable, traits<Types...>::is_trivially_move_assignable,
                           Types...> {
protected:
  using base = move_assignable_base<traits<Types...>::is_move_assignable,
                                    traits<Types...>::is_trivially_move_assignable, Types...>;
  using base::base;

  constexpr copy_assignable_base &operator=(copy_assignable_base const &other) {
    if (other.current_index == this->current_index) {
      if (other.current_index == variant_npos) {
        return *this;
      } else {
        copy_assign_funcs[other.current_index](&other.storage, &(this->storage));
      }
    } else if (other.current_index == variant_npos) {
      this->destruct();
      this->current_index = variant_npos;
    } else {
      (get_copy_assign_diff_index_funcs(other.current_index))(this, other);
    }
    return *this;
  }

  copy_assignable_base(copy_assignable_base &&) = default;
  copy_assignable_base(copy_assignable_base const &) = default;
  copy_assignable_base &operator=(copy_assignable_base &&) = default;

private:
  using copy_assign_func_t = void (*)(void const *source_stg, void *this_stg);
  constexpr static copy_assign_func_t copy_assign_funcs[sizeof...(Types)] = {&util::copy_assign_call<Types>...};

  template <typename T, size_t I>
  static void copy_assign_diff_index_call(copy_assignable_base *this_, const copy_assignable_base &rhs) {
    if constexpr (std::is_nothrow_copy_constructible_v<T> ||
                  !std::is_nothrow_move_constructible_v<T>) { // equivalent to emplace
      this_->destruct();
      this_->current_index = variant_npos;
      new (&(this_->storage)) T(rhs.storage.get_ref(in_place_index<I>));
      this_->current_index = I;
    } else {
      this_->operator=(copy_assignable_base(rhs));
    }
  }

  using copy_assign_diff_index_funcs_t = void (*)(copy_assignable_base *this_, copy_assignable_base const &rhs);

  constexpr copy_assign_diff_index_funcs_t get_copy_assign_diff_index_funcs(size_t i) {
    using Indices = std::make_index_sequence<sizeof...(Types)>;
    return get_copy_assign_diff_index_funcs_impl(i, Indices{});
  }

  template <std::size_t... Is>
  constexpr copy_assign_diff_index_funcs_t get_copy_assign_diff_index_funcs_impl(size_t i, std::index_sequence<Is...>) {
    return copy_assign_diff_index_funcs<Is...>[i];
  }

  template <std::size_t... Is>
  static constexpr copy_assign_diff_index_funcs_t copy_assign_diff_index_funcs[sizeof...(Types)] = {
      &copy_assign_diff_index_call<Types, Is>...};
};

template <bool is_default_constructible, typename... Types>
struct default_constructible_base : copy_assignable_base<traits<Types...>::is_copy_assignable,
                                                         traits<Types...>::is_trivially_copy_assignable, Types...> {
protected:
  using base = copy_assignable_base<traits<Types...>::is_copy_assignable,
                                    traits<Types...>::is_trivially_copy_assignable, Types...>;
  using base::base;
  constexpr default_constructible_base() = delete;
};

template <typename... Types>
struct default_constructible_base<true, Types...>
    : copy_assignable_base<traits<Types...>::is_copy_assignable, traits<Types...>::is_trivially_copy_assignable,
                           Types...> {
protected:
  using base = copy_assignable_base<traits<Types...>::is_copy_assignable,
                                    traits<Types...>::is_trivially_copy_assignable, Types...>;
  using base::base;

  constexpr default_constructible_base() noexcept(std::is_nothrow_default_constructible_v<type_by_index_t<0, Types...>>)
      : default_constructible_base(in_place_index<0>){};
};

template <typename... Types>
struct variant : private default_constructible_base<traits<Types...>::is_default_constructible, Types...> {
  using base = default_constructible_base<traits<Types...>::is_default_constructible, Types...>;

  template <size_t N, typename = std::enable_if_t<(N < sizeof...(Types))>>
  using variant_type_by_index_v = type_by_index_t<N, Types...>;

  template <typename T> static constexpr size_t variant_index_by_type_v = index_by_type_v<T, Types...>;

  template <typename T> static constexpr bool is_in_place_t = is_in_place_t_impl<T>::value;

  template <typename T> static constexpr bool unique_type = counter<T, Types...>::value == 1;

  template <typename T, typename = std::enable_if_t<!std::is_same_v<std::decay_t<T>, variant>>,
            typename = std::enable_if_t<!std::is_same_v<std::remove_cv_t<std::remove_reference_t<T>>, variant>>,
            typename = std::enable_if_t<!is_in_place_t<std::remove_cv_t<std::remove_reference_t<T>>>>,
            typename T_j = best_choice<T, variant>, typename = std::enable_if_t<unique_type<T_j>>,
            typename = std::enable_if_t<std::is_constructible_v<T_j, T &&>>>
  constexpr variant(T &&t) noexcept(std::is_nothrow_constructible_v<T_j, T &&>)
      : variant(in_place_index<variant_index_by_type_v<T_j>>, std::forward<T>(t)) {}

  template <size_t I, typename... Args,
            typename = std::enable_if_t<std::is_constructible_v<variant_type_by_index_v<I>, Args &&...>>>
  constexpr explicit variant(in_place_index_t<I> in_place, Args &&...args)
      : base(in_place, std::forward<Args>(args)...) {}

  template <class T, class... Args, typename = std::enable_if_t<unique_type<T>>,
            typename = std::enable_if_t<std::is_constructible_v<T, Args &&...>>>
  constexpr explicit variant(in_place_type_t<T>, Args &&...args)
      : variant(in_place_index<variant_index_by_type_v<T>>, std::forward<Args>(args)...) {}

  constexpr variant() = default;
  variant(variant &&) = default;
  variant(variant const &) = default;
  variant &operator=(const variant &) = default;
  variant &operator=(variant &&) = default;

  template <typename T, typename = std::enable_if_t<!std::is_same_v<std::decay_t<T>, variant>>,
            typename = std::enable_if_t<!std::is_same_v<std::remove_cv_t<std::remove_reference_t<T>>, variant>>,
            typename T_j = best_choice<T, variant>, size_t j = variant_index_by_type_v<T_j>,
            typename = std::enable_if_t<unique_type<T_j>>,
            typename = std::enable_if_t<std::is_constructible_v<T_j, T &&>>>
  variant &operator=(T &&t) noexcept(std::is_nothrow_assignable_v<T_j &, T> &&std::is_nothrow_constructible_v<T_j, T>) {
    if (index() == j) {
      var_get_ref(in_place_index<j>) = std::forward<T>(t);
    } else {
      if constexpr (std::is_nothrow_constructible_v<T_j, T> || !std::is_nothrow_move_constructible_v<T_j>) {
        emplace<j>(std::forward<T>(t));
      } else {
        this->operator=(variant(std::forward<T>(t)));
      }
    }
    return *this;
  }

  constexpr size_t index() const noexcept { return this->current_index; }

  constexpr bool valueless_by_exception() const noexcept { return index() == variant_npos; }

  void swap(variant &rhs) noexcept(((std::is_nothrow_move_constructible_v<Types> &&
                                     std::is_nothrow_swappable_v<Types>)&&...)) {
    if (rhs.index() == index()) {
      if (!valueless_by_exception()) {
        swap_funcs[index()](&rhs.storage, &(this->storage));
      }
    } else {
      if (rhs.valueless_by_exception()) {
        rhs.move_storage(std::move(*this));
        this->destruct();
        this->current_index = variant_npos;
      } else if (valueless_by_exception()) {
        this->move_storage(std::move(rhs));
        rhs.destruct();
        rhs.current_index = variant_npos;
      } else {
        variant temp = std::move(rhs);
        rhs = std::move(*this);
        *this = std::move(temp);
      }
    }
  }

  template <class T, class... Args, typename = std::enable_if_t<std::is_constructible_v<T, Args...>>,
            typename = std::enable_if_t<unique_type<T>>>
  T &emplace(Args &&...args) {
    return emplace<index_by_type_v<T, Types...>>(std::forward<Args>(args)...);
  }

  template <size_t I, class... Args, typename T = variant_type_by_index_v<I>,
            typename = std::enable_if_t<std::is_constructible_v<T, Args...>>>
  variant_alternative_t<I, variant> &emplace(Args &&...args) {
    this->destruct();
    this->current_index = variant_npos;
    new (&(this->storage)) T(std::forward<Args>(args)...);
    this->current_index = I;
    return var_get_ref(in_place_index<I>);
  }

private:
  using swap_func_t = void (*)(void *source_stg, void *this_stg);
  constexpr static swap_func_t swap_funcs[sizeof...(Types)] = {&util::swap_call<Types>...};

  template <size_t Num> constexpr decltype(auto) var_get_ptr(in_place_index_t<Num> num) {
    return this->storage.get_ptr(num);
  }
  template <size_t Num> constexpr decltype(auto) var_get_ptr(in_place_index_t<Num> num) const {
    return this->storage.get_ptr(num);
  }

  template <size_t Num> constexpr decltype(auto) var_get_ref(in_place_index_t<Num> num) {
    return this->storage.get_ref(num);
  }
  template <size_t Num> constexpr decltype(auto) var_get_ref(in_place_index_t<Num> num) const {
    return this->storage.get_ref(num);
  }

  template <size_t I, typename... Ts> friend constexpr variant_alternative_t<I, variant<Ts...>> &get(variant<Ts...> &v);
  template <size_t I, typename... Ts>
  friend constexpr variant_alternative_t<I, variant<Ts...>> &&get(variant<Ts...> &&v);
  template <size_t I, typename... Ts>
  friend constexpr variant_alternative_t<I, variant<Ts...>> const &&get(variant<Ts...> const &&v);
  template <size_t I, typename... Ts>
  friend constexpr variant_alternative_t<I, variant<Ts...>> const &get(variant<Ts...> const &v);

  template <typename T, typename... Ts> friend constexpr T &get(variant<Ts...> &v);
  template <typename T, typename... Ts> friend constexpr T &&get(variant<Ts...> &&v);
  template <typename T, typename... Ts> friend constexpr T const &&get(variant<Ts...> const &&v);
  template <typename T, typename... Ts> friend constexpr T const &get(variant<Ts...> const &v);

  template <size_t I, class... Ts>
  friend constexpr std::add_pointer_t<variant_alternative_t<I, variant<Ts...>>> get_if(variant<Ts...> *pv) noexcept;
  template <size_t I, class... Ts>
  friend constexpr std::add_pointer_t<const variant_alternative_t<I, variant<Types...>>>
  get_if(const variant<Ts...> *pv) noexcept;
};

template <class T, class... Types> constexpr bool holds_alternative(const variant<Types...> &v) noexcept {
  return v.index() == index_by_type_v<T, Types...>;
}
struct bad_variant_access : std::exception {
  char const *what() const noexcept override { return "empty variant used"; }
};

template <size_t I, typename... Types>
constexpr variant_alternative_t<I, variant<Types...>> &get(variant<Types...> &v) {
  if (v.index() == I) {
    return v.var_get_ref(in_place_index<I>);
  } else {
    throw bad_variant_access();
  }
}
template <size_t I, class... Types> constexpr variant_alternative_t<I, variant<Types...>> &&get(variant<Types...> &&v) {
  if (v.index() == I) {
    return std::move(v.var_get_ref(in_place_index<I>));
  } else {
    throw bad_variant_access();
  }
}
template <size_t I, class... Types>
constexpr const variant_alternative_t<I, variant<Types...>> &get(const variant<Types...> &v) {
  if (v.index() == I) {
    return v.var_get_ref(in_place_index<I>);
  } else {
    throw bad_variant_access();
  }
}
template <size_t I, class... Types>
constexpr const variant_alternative_t<I, variant<Types...>> &&get(const variant<Types...> &&v) {
  if (v.index() == I) {
    return std::move(v.var_get_ref(in_place_index<I>));
  } else {
    throw bad_variant_access();
  }
}

template <class T, class... Types> constexpr T &get(variant<Types...> &v) {
  return get<index_by_type_v<T, Types...>, Types...>(v);
}
template <class T, class... Types> constexpr T &&get(variant<Types...> &&v) {
  return get<index_by_type_v<T, Types...>, Types...>(std::move(v));
}
template <class T, class... Types> constexpr const T &get(const variant<Types...> &v) {
  return get<index_by_type_v<T, Types...>, Types...>(v);
}
template <class T, class... Types> constexpr const T &&get(const variant<Types...> &&v) {
  return get<index_by_type_v<T, Types...>, Types...>(std::move(v));
}

template <typename Return_Type, typename Caller_Type, typename Visitor, typename Tuple_Picked, typename Index_Seq,
          typename Tuple_Not_Picked>
struct visit_function_holder;

template <typename Return_Type, typename Caller_Type, typename Visitor, typename... Picked_Variants,
          size_t... Index_Seq, typename Current_Variant, typename... Not_Picked_Variants>
struct visit_function_holder<Return_Type, Caller_Type, Visitor, std::tuple<Picked_Variants...>,
                             std::index_sequence<Index_Seq...>, std::tuple<Current_Variant, Not_Picked_Variants...>> {

  using pick_current_index_func_t = Caller_Type (*)(Not_Picked_Variants &&...);

  static constexpr Caller_Type get_caller(Current_Variant &&var, Not_Picked_Variants &&...not_picked) {
    using Indices = std::make_index_sequence<variant_size_v<std::remove_reference_t<Current_Variant>>>;
    return get_pick_current_index_func(var.index(), Indices{})(std::forward<Not_Picked_Variants>(not_picked)...);
  }

  template <std::size_t... Is>
  static constexpr pick_current_index_func_t get_pick_current_index_func(size_t i, std::index_sequence<Is...>) {
    return pick_current_index_func<Is...>[i];
  }

  template <size_t I> static constexpr Caller_Type pick_current_index_func_impl(Not_Picked_Variants &&...not_picked) {
    return visit_function_holder<Return_Type, Caller_Type, Visitor, std::tuple<Picked_Variants..., Current_Variant>,
                                 std::index_sequence<Index_Seq..., I>, std::tuple<Not_Picked_Variants...>>::
        get_caller(std::forward<Not_Picked_Variants>(not_picked)...);
  }
  template <std::size_t... Is>
  static constexpr pick_current_index_func_t pick_current_index_func[sizeof...(Is)] = {
      &pick_current_index_func_impl<Is>...};
};

template <typename Return_Type, typename Caller_Type, typename Visitor, typename... Picked_Variants,
          size_t... Index_Seq>
struct visit_function_holder<Return_Type, Caller_Type, Visitor, std::tuple<Picked_Variants...>,
                             std::index_sequence<Index_Seq...>, std::tuple<>> {

  static constexpr Return_Type result_function(Visitor &&vis, Picked_Variants &&...vars) {
    return std::forward<Visitor>(vis)(get<Index_Seq>(std::forward<Picked_Variants>(vars))...);
  }

  static constexpr Caller_Type get_caller() { return &result_function; }
};

template <typename Visitor, typename... Variants> constexpr decltype(auto) visit(Visitor &&vis, Variants &&...vars) {
  if ((vars.valueless_by_exception() || ...))
    throw bad_variant_access();

  using Return_Type = decltype(std::forward<Visitor>(vis)(get<0>(std::forward<Variants>(vars))...));
  using invoke_func_t = Return_Type (*)(Visitor && vis, Variants && ...variant);

  return (*(visit_function_holder<Return_Type, invoke_func_t, Visitor, std::tuple<>, std::index_sequence<>,
                                  std::tuple<Variants...>>::get_caller(std::forward<Variants>(vars)...)))(
      std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}

template <size_t I, class... Types>
constexpr std::add_pointer_t<variant_alternative_t<I, variant<Types...>>> get_if(variant<Types...> *pv) noexcept {
  if (pv && pv->index() == I) {
    return pv->var_get_ptr(in_place_index<I>);
  } else {
    return nullptr;
  }
}

template <size_t I, class... Types>
constexpr std::add_pointer_t<const variant_alternative_t<I, variant<Types...>>>
get_if(const variant<Types...> *pv) noexcept {
  if (pv && pv->index() == I) {
    return pv->var_get_ptr(in_place_index<I>);
  } else {
    return nullptr;
  }
}
template <class T, class... Types> constexpr std::add_pointer_t<T> get_if(variant<Types...> *pv) noexcept {
  return get_if<index_by_type_v<T, Types...>>(pv);
}
template <class T, class... Types> constexpr std::add_pointer_t<const T> get_if(const variant<Types...> *pv) noexcept {
  return get_if<index_by_type_v<T, Types...>>(pv);
}

template <size_t N, typename... Types>
constexpr bool loop_equal(const variant<Types...> &v, const variant<Types...> &w) {
  if (N == v.index()) {
    return get<N>(v) == get<N>(w);
  } else {
    if constexpr (N + 1 < variant_size_v<variant<Types...>>)
      return loop_equal<N + 1>(v, w);
    else
      return false;
  }
}
template <size_t N, typename... Types>
constexpr bool loop_not_equal(const variant<Types...> &v, const variant<Types...> &w) {
  if (N == v.index()) {
    return get<N>(v) != get<N>(w);
  } else {
    if constexpr (N + 1 < variant_size_v<variant<Types...>>)
      return loop_not_equal<N + 1>(v, w);
    else
      return false;
  }
}
template <size_t N, typename... Types>
constexpr bool loop_less(const variant<Types...> &v, const variant<Types...> &w) {
  if (N == v.index()) {
    return get<N>(v) < get<N>(w);
  } else {
    if constexpr (N + 1 < variant_size_v<variant<Types...>>)
      return loop_less<N + 1>(v, w);
    else
      return false;
  }
}
template <size_t N, typename... Types>
constexpr bool loop_greater(const variant<Types...> &v, const variant<Types...> &w) {
  if (N == v.index()) {
    return get<N>(v) > get<N>(w);
  } else {
    if constexpr (N + 1 < variant_size_v<variant<Types...>>)
      return loop_greater<N + 1>(v, w);
    else
      return false;
  }
}
template <size_t N, typename... Types>
constexpr bool loop_less_equal(const variant<Types...> &v, const variant<Types...> &w) {
  if (N == v.index()) {
    return get<N>(v) <= get<N>(w);
  } else {
    if constexpr (N + 1 < variant_size_v<variant<Types...>>)
      return loop_less_equal<N + 1>(v, w);
    else
      return false;
  }
}
template <size_t N, typename... Types>
constexpr bool loop_greater_equal(const variant<Types...> &v, const variant<Types...> &w) {
  if (N == v.index()) {
    return get<N>(v) >= get<N>(w);
  } else {
    if constexpr (N + 1 < variant_size_v<variant<Types...>>)
      return loop_greater_equal<N + 1>(v, w);
    else
      return false;
  }
}

template <class... Types> constexpr bool operator==(const variant<Types...> &v, const variant<Types...> &w) {
  if (v.index() != w.index())
    return false;
  if (v.valueless_by_exception())
    return true;
  return loop_equal<0>(v, w);
}
template <class... Types> constexpr bool operator!=(const variant<Types...> &v, const variant<Types...> &w) {
  if (v.index() != w.index())
    return true;
  if (v.valueless_by_exception())
    return false;
  return loop_not_equal<0>(v, w);
}
template <class... Types> constexpr bool operator<(const variant<Types...> &v, const variant<Types...> &w) {
  if (w.valueless_by_exception()) {
    return false;
  }
  if (v.valueless_by_exception()) {
    return true;
  }
  if (v.index() < w.index()) {
    return true;
  }
  if (v.index() > w.index()) {
    return false;
  }
  return loop_less<0>(v, w);
}
template <class... Types> constexpr bool operator>(const variant<Types...> &v, const variant<Types...> &w) {
  if (v.valueless_by_exception()) {
    return false;
  }
  if (w.valueless_by_exception()) {
    return true;
  }
  if (v.index() > w.index()) {
    return true;
  }
  if (v.index() < w.index()) {
    return false;
  }
  return loop_greater<0>(v, w);
}
template <class... Types> constexpr bool operator<=(const variant<Types...> &v, const variant<Types...> &w) {
  if (v.valueless_by_exception()) {
    return true;
  }
  if (w.valueless_by_exception()) {
    return false;
  }
  if (v.index() < w.index()) {
    return true;
  }
  if (v.index() > w.index()) {
    return false;
  }
  return loop_less_equal<0>(v, w);
}
template <class... Types> constexpr bool operator>=(const variant<Types...> &v, const variant<Types...> &w) {
  if (w.valueless_by_exception()) {
    return true;
  }
  if (v.valueless_by_exception()) {
    return false;
  }
  if (v.index() > w.index()) {
    return true;
  }
  if (v.index() < w.index()) {
    return false;
  }
  return loop_greater_equal<0>(v, w);
}
