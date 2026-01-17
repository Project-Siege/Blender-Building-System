bl_info = {
    "name": "Regenesis Rust-Style Build Pie (B1 + Sockets v4)",
    "author": "Nomadic_Jester",
    "version": (1, 4, 0),
    "blender": (3, 0, 0),
    "location": "3D View",
    "description": "Rust-like 3m building with foundation-local snapping, triangle edge sockets, rotation, and proper ceiling snapping",
    "category": "3D View",
}

from math import atan2, radians, sqrt

import bmesh
import bpy
from bpy_extras import view3d_utils
from mathutils import Matrix, Vector

UNIT = 3.0
TRI_H = sqrt(3) / 2 * UNIT
THICK = 0.2

FOUNDATION_NAMES = ("Square_Foundation", "Tri_Foundation")

# ---------------- Mesh creation ----------------


def create_mesh_object(name, verts, faces):
    mesh = bpy.data.meshes.new(name)
    mesh.from_pydata(verts, [], faces)
    mesh.update()
    obj = bpy.data.objects.new(name, mesh)
    bpy.context.collection.objects.link(obj)
    return obj


def place_triangle_on_triangle_edge(hit_world, foundation_obj, tri_obj):
    # Find closest edge on the existing triangle
    edge, edge_point = find_closest_tri_edge(hit_world, foundation_obj)
    if edge is None:
        return

    a, b = edge
    edge_dir = b - a
    edge_dir.z = 0.0
    if edge_dir.length < 1e-6:
        return
    edge_dir.normalize()

    # Compute outward normal
    edge_normal = Vector((-edge_dir.y, edge_dir.x, 0.0))
    edge_normal.normalize()

    # Ensure normal points outward from the triangle
    center = get_foundation_center_world(foundation_obj)
    to_edge = edge_point - center
    if to_edge.dot(edge_normal) < 0:
        edge_normal = -edge_normal

    # Rotate new triangle so its flat edge aligns to the target edge
    angle = atan2(edge_normal.y, edge_normal.x) - radians(90)
    tri_obj.rotation_euler = (0.0, 0.0, angle)

    # Snap flat edge midpoint to the existing triangle's edge midpoint
    edge_midpoint = (a + b) / 2.0
    edge_midpoint.z = foundation_obj.location.z

    tri_obj.location = edge_midpoint


def extrude_solid(obj, thickness, along="Z"):
    me = obj.data
    bm = bmesh.new()
    bm.from_mesh(me)
    bmesh.ops.recalc_face_normals(bm, faces=bm.faces)
    geom = bmesh.ops.extrude_face_region(bm, geom=bm.faces)["geom"]
    verts = [e for e in geom if isinstance(e, bmesh.types.BMVert)]
    if along == "Z":
        offset = Vector((0, 0, thickness))
    elif along == "Y":
        offset = Vector((0, thickness, 0))
    else:
        offset = Vector((0, 0, thickness))
    for v in verts:
        v.co += offset
    bm.to_mesh(me)
    bm.free()


def make_square_foundation():
    s = UNIT / 2.0
    verts = [
        Vector((-s, -s, 0.0)),
        Vector((s, -s, 0.0)),
        Vector((s, s, 0.0)),
        Vector((-s, s, 0.0)),
    ]
    faces = [(0, 1, 2, 3)]
    obj = create_mesh_object("Square_Foundation", verts, faces)
    extrude_solid(obj, THICK, along="Z")
    obj.location = (0, 0, 0)
    return obj


def make_tri_foundation():
    h = TRI_H
    verts = [
        Vector((-UNIT / 2, 0.0, 0.0)),
        Vector((UNIT / 2, 0.0, 0.0)),
        Vector((0.0, h, 0.0)),
    ]
    faces = [(0, 1, 2)]
    obj = create_mesh_object("Tri_Foundation", verts, faces)
    extrude_solid(obj, THICK, along="Z")
    obj.location = (0, 0, 0)
    return obj


def make_ceiling():
    s = UNIT / 2.0
    verts = [
        Vector((-s, -s, 0.0)),
        Vector((s, -s, 0.0)),
        Vector((s, s, 0.0)),
        Vector((-s, s, 0.0)),
    ]
    faces = [(0, 1, 2, 3)]
    obj = create_mesh_object("Ceiling", verts, faces)
    extrude_solid(obj, THICK, along="Z")
    obj.location = (0, 0, 0)
    return obj


def make_tri_ceiling():
    h = TRI_H
    verts = [
        Vector((-UNIT / 2, 0.0, 0.0)),
        Vector((UNIT / 2, 0.0, 0.0)),
        Vector((0.0, h, 0.0)),
    ]
    faces = [(0, 1, 2)]
    obj = create_mesh_object("Tri_Ceiling", verts, faces)
    extrude_solid(obj, THICK, along="Z")
    obj.location = (0, 0, 0)
    return obj


def make_wall(full_height=True):
    h = UNIT if full_height else UNIT / 2.0
    w = UNIT
    s = w / 2.0
    verts = [
        Vector((-s, 0.0, 0.0)),
        Vector((s, 0.0, 0.0)),
        Vector((s, 0.0, h)),
        Vector((-s, 0.0, h)),
    ]
    faces = [(0, 1, 2, 3)]
    name = "Wall" if full_height else "Half_Wall"
    obj = create_mesh_object(name, verts, faces)
    extrude_solid(obj, THICK, along="Y")
    obj.location = (0, 0, 0)
    return obj


def make_doorway():
    h = UNIT
    w = UNIT
    s = w / 2.0
    door_h = UNIT * 0.75
    door_w = UNIT * 0.5

    verts = [
        Vector((-s, 0.0, 0.0)),
        Vector((-door_w / 2, 0.0, 0.0)),
        Vector((door_w / 2, 0.0, 0.0)),
        Vector((s, 0.0, 0.0)),
        Vector((-door_w / 2, 0.0, door_h)),
        Vector((door_w / 2, 0.0, door_h)),
        Vector((-s, 0.0, h)),
        Vector((s, 0.0, h)),
    ]
    faces = [
        (0, 1, 4, 6),
        (2, 3, 7, 5),
        (4, 5, 7, 6),
    ]
    obj = create_mesh_object("Doorway", verts, faces)
    extrude_solid(obj, THICK, along="Y")
    obj.location = (0, 0, 0)
    return obj


def make_window_frame():
    h = UNIT
    w = UNIT
    s = w / 2.0
    win_bottom = UNIT * 0.3
    win_top = UNIT * 0.7
    win_w = UNIT * 0.6

    verts = [
        Vector((-s, 0.0, 0.0)),
        Vector((s, 0.0, 0.0)),
        Vector((-win_w / 2, 0.0, win_bottom)),
        Vector((win_w / 2, 0.0, win_bottom)),
        Vector((-win_w / 2, 0.0, win_top)),
        Vector((win_w / 2, 0.0, win_top)),
        Vector((-s, 0.0, h)),
        Vector((s, 0.0, h)),
    ]
    faces = [
        (0, 1, 3, 2),
        (2, 3, 1, 0),
        (0, 2, 4, 6),
        (3, 1, 7, 5),
        (4, 5, 7, 6),
    ]
    obj = create_mesh_object("Window_Frame", verts, faces)
    extrude_solid(obj, THICK, along="Y")
    obj.location = (0, 0, 0)
    return obj


def make_wall_frame():
    h = UNIT
    w = UNIT
    s = w / 2.0
    frame_thick = 0.15

    verts = [
        Vector((-s, 0.0, 0.0)),
        Vector((s, 0.0, 0.0)),
        Vector((s, 0.0, h)),
        Vector((-s, 0.0, h)),
        Vector((-s + frame_thick, 0.0, frame_thick)),
        Vector((s - frame_thick, 0.0, frame_thick)),
        Vector((s - frame_thick, 0.0, h - frame_thick)),
        Vector((-s + frame_thick, 0.0, h - frame_thick)),
    ]
    faces = [
        (0, 1, 5, 4),
        (1, 2, 6, 5),
        (2, 3, 7, 6),
        (3, 0, 4, 7),
    ]
    obj = create_mesh_object("Wall_Frame", verts, faces)
    extrude_solid(obj, THICK, along="Y")
    obj.location = (0, 0, 0)
    return obj


# ---------------- Raycast helpers ----------------


def raycast_from_mouse(context, event):
    region = context.region
    rv3d = context.region_data
    coord = (event.mouse_region_x, event.mouse_region_y)
    view_vector = view3d_utils.region_2d_to_vector_3d(region, rv3d, coord)
    ray_origin = view3d_utils.region_2d_to_origin_3d(region, rv3d, coord)
    ray_target = ray_origin + view_vector * 1000.0
    result, location, normal, index, obj, matrix = context.scene.ray_cast(
        context.view_layer.depsgraph,
        ray_origin,
        (ray_target - ray_origin).normalized(),
        distance=1000.0,
    )
    if result:
        return location, normal, obj
    return None, None, None


def prioritized_raycast(context, event):
    hit_world, normal, obj = raycast_from_mouse(context, event)
    if obj and get_piece_type(obj) in {
        "WALL",
        "HALF_WALL",
        "DOORWAY",
        "WINDOW_FRAME",
        "WALL_FRAME",
    }:
        return hit_world, normal, obj
    return hit_world, normal, obj


def ground_intersection(context, event, ground_z=0.0):
    region = context.region
    rv3d = context.region_data
    coord = (event.mouse_region_x, event.mouse_region_y)
    view_vector = view3d_utils.region_2d_to_vector_3d(region, rv3d, coord)
    ray_origin = view3d_utils.region_2d_to_origin_3d(region, rv3d, coord)
    if abs(view_vector.z) < 1e-6:
        return None
    t = (ground_z - ray_origin.z) / view_vector.z
    if t < 0:
        return None
    hit = ray_origin + view_vector * t
    return hit


# ---------------- Foundation helpers ----------------


def is_foundation(obj):
    if obj is None:
        return False
    name = obj.name
    return any(n in name for n in FOUNDATION_NAMES)


def is_square_foundation(obj):
    return obj is not None and "Square_Foundation" in obj.name


def is_tri_foundation(obj):
    return obj is not None and "Tri_Foundation" in obj.name


def get_piece_type(obj):
    if obj is None:
        return None
    name = obj.name
    if "Square_Foundation" in name:
        return "SQUARE_FOUNDATION"
    if "Tri_Foundation" in name:
        return "TRI_FOUNDATION"
    if "Tri_Ceiling" in name:
        return "TRI_CEILING"
    if "Ceiling" in name and "Tri" not in name:
        return "CEILING"
    if "Half_Wall" in name:
        return "HALF_WALL"
    if "Window_Frame" in name:
        return "WINDOW_FRAME"
    if "Wall_Frame" in name:
        return "WALL_FRAME"
    if "Doorway" in name:
        return "DOORWAY"
    if "Wall" in name:
        return "WALL"
    return None


def snap_to_world_grid(pos):
    return Vector((round(pos.x / UNIT) * UNIT, round(pos.y / UNIT) * UNIT, 0.0))


def get_foundation_center_world(foundation_obj):
    return foundation_obj.matrix_world @ Vector((0.0, 0.0, 0.0))


def snap_on_square_foundation(hit_world, foundation_obj):
    inv = foundation_obj.matrix_world.inverted()
    local = inv @ hit_world
    local_snapped = Vector(
        (round(local.x / UNIT) * UNIT, round(local.y / UNIT) * UNIT, 0.0)
    )
    snapped_world = foundation_obj.matrix_world @ local_snapped
    return snapped_world


def snap_on_tri_foundation_grid(hit_world, foundation_obj):
    inv = foundation_obj.matrix_world.inverted()
    local = inv @ hit_world
    u = Vector((UNIT, 0.0, 0.0))
    v = Vector((UNIT / 2.0, TRI_H, 0.0))
    mat2 = Matrix(((u.x, v.x), (u.y, v.y)))
    det = mat2[0][0] * mat2[1][1] - mat2[0][1] * mat2[1][0]
    if abs(det) < 1e-6:
        return foundation_obj.matrix_world @ local
    inv2 = Matrix(
        ((mat2[1][1] / det, -mat2[0][1] / det), (-mat2[1][0] / det, mat2[0][0] / det))
    )
    xy = Vector((local.x, local.y))
    ab = inv2 @ xy
    a_snapped = round(ab.x)
    b_snapped = round(ab.y)
    snapped_xy = u.xy * a_snapped + v.xy * b_snapped
    local_snapped = Vector((snapped_xy.x, snapped_xy.y, 0.0))
    snapped_world = foundation_obj.matrix_world @ local_snapped
    return snapped_world


def get_tri_edges_world(foundation_obj):
    h = TRI_H
    local_verts = [
        Vector((-UNIT / 2, 0.0, 0.0)),
        Vector((UNIT / 2, 0.0, 0.0)),
        Vector((0.0, h, 0.0)),
    ]
    world_verts = [foundation_obj.matrix_world @ v for v in local_verts]
    edges = [
        (world_verts[0], world_verts[1]),
        (world_verts[1], world_verts[2]),
        (world_verts[2], world_verts[0]),
    ]
    return edges


def find_closest_tri_edge(hit_world, foundation_obj):
    edges = get_tri_edges_world(foundation_obj)
    best_edge = None
    best_point = None
    best_dist = 1e9
    for a, b in edges:
        cp = closest_point_on_segment(hit_world, a, b)
        d = (hit_world - cp).length
        if d < best_dist:
            best_dist = d
            best_edge = (a, b)
            best_point = cp
    return best_edge, best_point


# ---------------- Edge-based socket snapping ----------------


def get_square_edges_world(foundation_obj):
    s = UNIT / 2.0
    local_verts = [
        Vector((-s, -s, 0.0)),
        Vector((s, -s, 0.0)),
        Vector((s, s, 0.0)),
        Vector((-s, s, 0.0)),
    ]
    world_verts = [foundation_obj.matrix_world @ v for v in local_verts]
    edges = [
        (world_verts[0], world_verts[1]),
        (world_verts[1], world_verts[2]),
        (world_verts[2], world_verts[3]),
        (world_verts[3], world_verts[0]),
    ]
    return edges


def closest_point_on_segment(p, a, b):
    ab = b - a
    t = (p - a).dot(ab) / (ab.dot(ab) + 1e-8)
    t = max(0.0, min(1.0, t))
    return a + ab * t


def find_closest_square_edge(hit_world, foundation_obj):
    edges = get_square_edges_world(foundation_obj)
    best_edge = None
    best_point = None
    best_dist = 1e9
    for a, b in edges:
        cp = closest_point_on_segment(hit_world, a, b)
        d = (hit_world - cp).length
        if d < best_dist:
            best_dist = d
            best_edge = (a, b)
            best_point = cp
    return best_edge, best_point


def place_triangle_on_square_edge(hit_world, foundation_obj, tri_obj):
    edge, edge_point = find_closest_square_edge(hit_world, foundation_obj)
    if edge is None:
        return

    a, b = edge
    edge_dir = b - a
    edge_dir.z = 0.0
    if edge_dir.length < 1e-6:
        return
    edge_dir.normalize()

    edge_normal = Vector((-edge_dir.y, edge_dir.x, 0.0))
    edge_normal.normalize()

    center = get_foundation_center_world(foundation_obj)
    to_edge = edge_point - center
    if to_edge.dot(edge_normal) < 0:
        edge_normal = -edge_normal

    angle = atan2(edge_normal.y, edge_normal.x) - radians(90)
    tri_obj.rotation_euler = (0.0, 0.0, angle)

    edge_midpoint = (a + b) / 2.0
    edge_midpoint.z = foundation_obj.location.z
    tri_obj.location = edge_midpoint


# ---------------- Wall / ceiling helpers ----------------


def get_wall_top_z(obj):
    zs = [v.co.z for v in obj.data.vertices]
    if not zs:
        return obj.location.z
    local_top = max(zs)
    return obj.location.z + local_top * obj.scale.z


# ---------------- Placement operator ----------------


class REGENESIS_OT_place_piece(bpy.types.Operator):
    bl_idname = "regenesis.place_piece"
    bl_label = "Place Build Piece"
    bl_options = {"REGISTER", "UNDO", "GRAB_CURSOR", "BLOCKING"}

    piece_type: bpy.props.StringProperty()

    def create_piece(self, context):
        if self.piece_type == "SQUARE_FOUNDATION":
            return make_square_foundation()
        elif self.piece_type == "TRI_FOUNDATION":
            return make_tri_foundation()
        elif self.piece_type == "WALL":
            return make_wall(True)
        elif self.piece_type == "HALF_WALL":
            return make_wall(False)
        elif self.piece_type == "CEILING":
            return make_ceiling()
        elif self.piece_type == "TRI_CEILING":
            return make_tri_ceiling()
        elif self.piece_type == "DOORWAY":
            return make_doorway()
        elif self.piece_type == "WINDOW_FRAME":
            return make_window_frame()
        elif self.piece_type == "WALL_FRAME":
            return make_wall_frame()
        return None

    def invoke(self, context, event):
        self.obj = self.create_piece(context)
        if not self.obj:
            return {"CANCELLED"}
        context.view_layer.objects.active = self.obj
        self.obj.select_set(True)
        context.window_manager.modal_handler_add(self)
        return {"RUNNING_MODAL"}

    def align_on_foundation(self, hit_world, normal, foundation_obj):
        is_wall_piece = self.piece_type in {
            "WALL",
            "HALF_WALL",
            "DOORWAY",
            "WINDOW_FRAME",
            "WALL_FRAME",
        }
        is_tri_piece = self.piece_type in {"TRI_FOUNDATION", "TRI_CEILING"}
        is_ceiling_piece = self.piece_type in {"CEILING", "TRI_CEILING"}

        hover_type = get_piece_type(foundation_obj)

        if (
            hover_type in {"WALL", "HALF_WALL", "DOORWAY", "WINDOW_FRAME", "WALL_FRAME"}
            and is_ceiling_piece
        ):
            top_z = get_wall_top_z(foundation_obj)
            self.obj.location = Vector(
                (foundation_obj.location.x, foundation_obj.location.y, top_z)
            )
            self.obj.rotation_euler = foundation_obj.rotation_euler
            return

        if is_square_foundation(foundation_obj):
            if is_tri_piece and self.piece_type == "TRI_FOUNDATION":
                place_triangle_on_square_edge(hit_world, foundation_obj, self.obj)
                return
            elif is_tri_piece and self.piece_type == "TRI_CEILING":
                snapped = snap_on_square_foundation(hit_world, foundation_obj)
                self.obj.location = snapped
                return
            elif is_wall_piece:
                edge, edge_point = find_closest_square_edge(hit_world, foundation_obj)
                if edge is None:
                    return
                a, b = edge
                edge_dir = b - a
                edge_dir.z = 0.0
                if edge_dir.length < 1e-6:
                    return
                edge_dir.normalize()
                edge_center = (a + b) / 2.0
                angle = atan2(edge_dir.y, edge_dir.x) - radians(90)
                self.obj.location = Vector(
                    (edge_center.x, edge_center.y, foundation_obj.location.z + THICK)
                )
                self.obj.rotation_euler = (0.0, 0.0, angle)
                return
            else:
                snapped = snap_on_square_foundation(hit_world, foundation_obj)
        elif is_tri_foundation(foundation_obj):
            if is_ceiling_piece and self.piece_type == "TRI_CEILING":
                snapped = snap_on_tri_foundation_grid(hit_world, foundation_obj)
                self.obj.location = snapped
                return
            elif is_wall_piece:
                edge, edge_point = find_closest_tri_edge(hit_world, foundation_obj)
                if edge is None:
                    return
                a, b = edge
                edge_dir = b - a
                edge_dir.z = 0.0
                if edge_dir.length < 1e-6:
                    return
                edge_dir.normalize()
                edge_center = (a + b) / 2.0
                angle = atan2(edge_dir.y, edge_dir.x) - radians(90)
                self.obj.location = Vector(
                    (edge_center.x, edge_center.y, foundation_obj.location.z + THICK)
                )
                self.obj.rotation_euler = (0.0, 0.0, angle)
                return
            else:
                if self.piece_type == "TRI_FOUNDATION":
                    place_triangle_on_triangle_edge(hit_world, foundation_obj, self.obj)
                    return
                snapped = snap_on_tri_foundation_grid(hit_world, foundation_obj)

        else:
            snapped = hit_world

        if is_wall_piece:
            center = get_foundation_center_world(foundation_obj)
            dir_vec = Vector((snapped.x - center.x, snapped.y - center.y, 0.0))
            if dir_vec.length > 1e-6:
                dir_vec.normalize()
                angle = atan2(dir_vec.x, dir_vec.y)
                self.obj.location = Vector((snapped.x, snapped.y, snapped.z))
                self.obj.rotation_euler = (0.0, 0.0, angle)
            else:
                self.obj.location = snapped
        else:
            up = Vector((0, 0, 1))
            axis = up.cross(normal)
            if axis.length > 1e-6:
                axis.normalize()
                angle = up.angle(normal)
                rot_mat = Matrix.Rotation(angle, 4, axis)
                self.obj.matrix_world = Matrix.Translation(snapped) @ rot_mat
            else:
                self.obj.location = snapped

    def modal(self, context, event):
        if event.type in {"RIGHTMOUSE", "ESC"}:
            bpy.data.objects.remove(self.obj, do_unlink=True)
            return {"CANCELLED"}

        if event.type == "R" and event.value == "PRESS":
            if self.piece_type in {"TRI_FOUNDATION", "TRI_CEILING"}:
                self.obj.rotation_euler.z += radians(60)
            else:
                self.obj.rotation_euler.z += radians(90)
            return {"RUNNING_MODAL"}

        if event.type == "MOUSEMOVE":
            hit_world, normal, hit_obj = prioritized_raycast(context, event)
            hover_type = get_piece_type(hit_obj)

            if hit_world is not None and hover_type in {
                "SQUARE_FOUNDATION",
                "TRI_FOUNDATION",
                "WALL",
                "HALF_WALL",
                "DOORWAY",
                "WINDOW_FRAME",
                "WALL_FRAME",
            }:
                self.align_on_foundation(hit_world, normal, hit_obj)
            else:
                if self.piece_type in {"SQUARE_FOUNDATION", "TRI_FOUNDATION"}:
                    ground_hit = ground_intersection(context, event, ground_z=0.0)
                    if ground_hit is not None:
                        snapped = snap_to_world_grid(ground_hit)
                        self.obj.location = snapped
                        self.obj.rotation_euler = (0.0, 0.0, 0.0)
                else:
                    pass

        if event.type == "LEFTMOUSE" and event.value == "PRESS":
            self.obj.select_set(True)
            return {"FINISHED"}

        return {"RUNNING_MODAL"}


# ---------------- Pie menu ----------------


class REGENESIS_MT_build_pie(bpy.types.Menu):
    bl_label = "Regenesis Build Pie"
    bl_idname = "REGENESIS_MT_build_pie"

    def draw(self, context):
        layout = self.layout
        pie = layout.menu_pie()
        pie.operator(
            "regenesis.place_piece", text="Square Foundation"
        ).piece_type = "SQUARE_FOUNDATION"
        pie.operator("regenesis.place_piece", text="Wall").piece_type = "WALL"
        pie.operator("regenesis.place_piece", text="Ceiling").piece_type = "CEILING"
        pie.operator("regenesis.place_piece", text="Doorway").piece_type = "DOORWAY"
        pie.operator(
            "regenesis.place_piece", text="Tri Foundation"
        ).piece_type = "TRI_FOUNDATION"
        pie.operator(
            "regenesis.place_piece", text="Window Frame"
        ).piece_type = "WINDOW_FRAME"
        pie.operator(
            "regenesis.place_piece", text="Tri Ceiling"
        ).piece_type = "TRI_CEILING"
        pie.operator("regenesis.place_piece", text="Half Wall").piece_type = "HALF_WALL"


class REGENESIS_OT_call_build_pie(bpy.types.Operator):
    bl_idname = "regenesis.call_build_pie"
    bl_label = "Call Regenesis Build Pie"

    def execute(self, context):
        bpy.ops.wm.call_menu_pie(name="REGENESIS_MT_build_pie")
        return {"FINISHED"}


# ---------------- Registration ----------------

classes = (
    REGENESIS_OT_place_piece,
    REGENESIS_MT_build_pie,
    REGENESIS_OT_call_build_pie,
)

addon_keymaps = []


def register():
    for c in classes:
        bpy.utils.register_class(c)
    wm = bpy.context.window_manager
    kc = wm.keyconfigs.addon
    if kc:
        km = kc.keymaps.new(name="3D View", space_type="VIEW_3D")
        kmi = km.keymap_items.new("regenesis.call_build_pie", type="Q", value="PRESS")
        addon_keymaps.append((km, kmi))


def unregister():
    for km, kmi in addon_keymaps:
        km.keymap_items.remove(kmi)
    addon_keymaps.clear()
    for c in reversed(classes):
        bpy.utils.unregister_class(c)


if __name__ == "__main__":
    register()
