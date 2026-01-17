bl_info = {
    "name": "Regenesis Building System",
    "author": "Nomadic_Jester",
    "version": (1, 4, 4),
    "blender": (3, 0, 0),
    "location": "3D View",
    "description": "Rust-like 3m building bullshit with triangle edge snapping and ceilings that actually work now",
    "category": "3D View",
}

from math import atan2, radians
import bmesh
import bpy
from bpy_extras import view3d_utils
from mathutils import Matrix, Vector

UNIT = 3.0
TRI_H = (3**0.5)/2 * UNIT     # yeah I know, sqrt(3) is prettier but whatever
THICK = 0.2

FOUNDATION_NAMES = ("Square_Foundation", "Tri_Foundation")


# ---------------- meshes ----------------

def create_mesh_object(name, verts, faces):
    mesh = bpy.data.meshes.new(name)
    mesh.from_pydata(verts, [], faces)
    mesh.update()
    obj = bpy.data.objects.new(name, mesh)
    bpy.context.collection.objects.link(obj)
    return obj


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
        Vector((-s, -s, 0)),
        Vector((s, -s, 0)),
        Vector((s, s, 0)),
        Vector((-s, s, 0)),
    ]
    obj = create_mesh_object("Square_Foundation", verts, [(0,1,2,3)])
    extrude_solid(obj, THICK, "Z")
    return obj


def make_tri_foundation():
    verts = [
        Vector((-UNIT/2, 0, 0)),
        Vector((UNIT/2, 0, 0)),
        Vector((0, TRI_H, 0)),
    ]
    obj = create_mesh_object("Tri_Foundation", verts, [(0,1,2)])
    extrude_solid(obj, THICK, "Z")
    return obj


def make_ceiling():
    s = UNIT / 2.0
    verts = [
        Vector((-s, 0, 0)),
        Vector((s, 0, 0)),
        Vector((s, UNIT, 0)),
        Vector((-s, UNIT, 0)),
    ]
    obj = create_mesh_object("Ceiling", verts, [(0,1,2,3)])
    extrude_solid(obj, THICK, "Z")
    return obj


def make_tri_ceiling():
    verts = [
        Vector((-UNIT/2, 0, 0)),
        Vector((UNIT/2, 0, 0)),
        Vector((0, TRI_H, 0)),
    ]
    obj = create_mesh_object("Tri_Ceiling", verts, [(0,1,2)])
    extrude_solid(obj, THICK, "Z")
    return obj


def make_wall(full_height=True):
    h = UNIT if full_height else UNIT/2
    s = UNIT / 2.0
    verts = [
        Vector((-s, 0, 0)),
        Vector((s, 0, 0)),
        Vector((s, 0, h)),
        Vector((-s, 0, h)),
    ]
    name = "Wall" if full_height else "Half_Wall"
    obj = create_mesh_object(name, verts, [(0,1,2,3)])
    extrude_solid(obj, THICK, "Y")
    return obj


def make_doorway():
    h = UNIT
    s = UNIT / 2.0
    door_h = UNIT * 0.75
    door_w = UNIT * 0.5

    verts = [
        (-s, 0, 0),
        (-door_w/2, 0, 0),
        (door_w/2, 0, 0),
        (s, 0, 0),
        (-door_w/2, 0, door_h),
        (door_w/2, 0, door_h),
        (-s, 0, h),
        (s, 0, h),
    ]
    faces = [
        (0,1,4,6),
        (2,3,7,5),
        (4,5,7,6),
    ]
    obj = create_mesh_object("Doorway", verts, faces)
    extrude_solid(obj, THICK, "Y")
    return obj


def make_window_frame():
    h = UNIT
    s = UNIT / 2.0
    win_bottom = UNIT * 0.3
    win_top = UNIT * 0.7
    win_w = UNIT * 0.6

    verts = [
        (-s, 0, 0),
        (s, 0, 0),
        (-win_w/2, 0, win_bottom),
        (win_w/2, 0, win_bottom),
        (-win_w/2, 0, win_top),
        (win_w/2, 0, win_top),
        (-s, 0, h),
        (s, 0, h),
    ]
    faces = [
        (0,1,3,2), (2,3,1,0),
        (0,2,4,6), (3,1,7,5),
        (4,5,7,6),
    ]
    obj = create_mesh_object("Window_Frame", verts, faces)
    extrude_solid(obj, THICK, "Y")
    return obj


def make_wall_frame():
    h = UNIT
    s = UNIT / 2.0
    ft = 0.15   # frame thick

    verts = [
        (-s, 0, 0), (s, 0, 0),
        (s, 0, h), (-s, 0, h),
        (-s+ft, 0, ft), (s-ft, 0, ft),
        (s-ft, 0, h-ft), (-s+ft, 0, h-ft),
    ]
    faces = [
        (0,1,5,4),
        (1,2,6,5),
        (2,3,7,6),
        (3,0,4,7),
    ]
    obj = create_mesh_object("Wall_Frame", verts, faces)
    extrude_solid(obj, THICK, "Y")
    return obj


# ---------------- helpers I always forget ----------------

def get_foundation_top_z(f):
    return f.location.z + THICK


def is_foundation(obj):
    return obj and any(n in obj.name for n in FOUNDATION_NAMES)


def is_square_foundation(obj):
    return obj and "Square_Foundation" in obj.name


def is_tri_foundation(obj):
    return obj and "Tri_Foundation" in obj.name


def get_piece_type(obj):
    if not obj: return None
    n = obj.name
    if "Square_Foundation" in n: return "SQUARE_FOUNDATION"
    if "Tri_Foundation" in n:    return "TRI_FOUNDATION"
    if "Tri_Ceiling" in n:       return "TRI_CEILING"
    if "Ceiling" in n:           return "CEILING"
    if "Half_Wall" in n:         return "HALF_WALL"
    if "Window_Frame" in n:      return "WINDOW_FRAME"
    if "Wall_Frame" in n:        return "WALL_FRAME"
    if "Doorway" in n:           return "DOORWAY"
    if "Wall" in n:              return "WALL"
    return None


def snap_world_grid(pos):
    return Vector((round(pos.x/UNIT)*UNIT, round(pos.y/UNIT)*UNIT, 0))


def snap_square(hit, f):
    rel = hit - f.location
    return f.location + Vector((round(rel.x/UNIT)*UNIT, round(rel.y/UNIT)*UNIT, 0))


def snap_tri_grid(hit, f):
    rel = hit - f.location
    u = Vector((UNIT, 0))
    v = Vector((UNIT/2, TRI_H))
    mat = Matrix(((u.x,v.x),(u.y,v.y)))
    det = mat[0][0]*mat[1][1] - mat[0][1]*mat[1][0]
    if abs(det) < 1e-6:
        return f.location + rel
    inv = Matrix(((mat[1][1]/det, -mat[0][1]/det), (-mat[1][0]/det, mat[0][0]/det)))
    ab = inv @ Vector((rel.x, rel.y))
    snapped = u*round(ab.x) + v*round(ab.y)
    return f.location + Vector((snapped.x, snapped.y, 0))


def get_edges_world(obj):
    mw = obj.matrix_world
    bm = bmesh.new()
    bm.from_mesh(obj.data)
    edges = [(mw@v.co for v in e.verts) for e in bm.edges]
    bm.free()
    return edges


def closest_on_seg(p, a, b):
    ab = b - a
    t = max(0, min(1, (p-a).dot(ab) / (ab.dot(ab)+1e-8)))
    return a + ab*t


def find_closest_edge(hit, obj):
    edges = get_edges_world(obj)
    best_d = float('inf')
    best = None, None
    for a,b in edges:
        cp = closest_on_seg(hit, a, b)
        d = (hit-cp).length
        if d < best_d:
            best_d = d
            best = (a,b), cp
    return best


def place_on_edge(hit, f, new):
    edge, point = find_closest_edge(hit, f)
    if not edge: return

    a, b = edge
    dir = (b-a).copy(); dir.z = 0
    if dir.length_squared < 1e-8: return
    dir.normalize()

    dir2d = Vector((dir.x, dir.y))
    norm2d = Vector((-dir2d.y, dir2d.x))

    cen2d = Vector((f.location.x, f.location.y))
    p2d = Vector((point.x, point.y))
    to_edge = p2d - cen2d

    if to_edge.dot(norm2d) < 0:
        norm2d *= -1

    ang = atan2(norm2d.y, norm2d.x) - radians(90)
    new.rotation_euler.z = ang

    mid = (a+b)/2
    mid.z = f.location.z          # same height for foundations dammit
    new.location = mid


def get_wall_top_z(obj):
    if "Half_Wall" in obj.name:
        return obj.location.z + UNIT/2
    return obj.location.z + UNIT


# ---------------- ray stuff ----------------

def ray_mouse(context, event):
    reg = context.region
    rv3d = context.region_data
    coord = event.mouse_region_x, event.mouse_region_y
    vec = view3d_utils.region_2d_to_vector_3d(reg, rv3d, coord)
    orig = view3d_utils.region_2d_to_origin_3d(reg, rv3d, coord)
    dir = vec.normalized()

    res, loc, _, _, obj, _ = context.scene.ray_cast(context.view_layer.depsgraph, orig, dir)
    return (loc, obj) if res else (None, None)


def prioritized_ray(context, event):
    hit, obj = ray_mouse(context, event)
    if obj and get_piece_type(obj) in {"WALL","HALF_WALL","DOORWAY","WINDOW_FRAME","WALL_FRAME"}:
        return hit, obj
    return hit, obj


def ground_hit(context, event, z=0):
    reg = context.region
    rv3d = context.region_data
    coord = event.mouse_region_x, event.mouse_region_y
    vec = view3d_utils.region_2d_to_vector_3d(reg, rv3d, coord)
    orig = view3d_utils.region_2d_to_origin_3d(reg, rv3d, coord)
    if abs(vec.z) < 1e-6: return None
    t = (z - orig.z) / vec.z
    if t < 0: return None
    return orig + vec * t


# ---------------- main operator ----------------

class REGENESIS_OT_place_piece(bpy.types.Operator):
    bl_idname = "regenesis.place_piece"
    bl_label = "Place Build Piece"
    bl_options = {'REGISTER', 'UNDO', 'GRAB_CURSOR', 'BLOCKING'}

    piece_type: bpy.props.StringProperty()

    def create_piece(self):
        d = {
            "SQUARE_FOUNDATION": make_square_foundation,
            "TRI_FOUNDATION": make_tri_foundation,
            "WALL": lambda: make_wall(True),
            "HALF_WALL": lambda: make_wall(False),
            "CEILING": make_ceiling,
            "TRI_CEILING": make_tri_ceiling,
            "DOORWAY": make_doorway,
            "WINDOW_FRAME": make_window_frame,
            "WALL_FRAME": make_wall_frame,
        }
        fn = d.get(self.piece_type)
        return fn() if fn else None

    def invoke(self, context, event):
        self.obj = self.create_piece()
        if not self.obj: return {'CANCELLED'}
        context.view_layer.objects.active = self.obj
        self.obj.select_set(True)
        context.window_manager.modal_handler_add(self)
        return {'RUNNING_MODAL'}

    def modal(self, context, event):
        if event.type in {'RIGHTMOUSE', 'ESC'}:
            bpy.data.objects.remove(self.obj, do_unlink=True)
            return {'CANCELLED'}

        if event.type == 'R' and event.value == 'PRESS':
            rot = radians(60) if self.piece_type in {"TRI_FOUNDATION","TRI_CEILING"} else radians(90)
            self.obj.rotation_euler.z += rot
            return {'RUNNING_MODAL'}

        if event.type == 'MOUSEMOVE':
            hit, obj = prioritized_ray(context, event)
            pt = get_piece_type(self.obj)
            is_wall_like = pt in {"WALL","HALF_WALL","DOORWAY","WINDOW_FRAME","WALL_FRAME"}
            is_tri = pt in {"TRI_FOUNDATION","TRI_CEILING"}
            is_ceil = pt in {"CEILING","TRI_CEILING"}

            if obj:
                ht = get_piece_type(obj)

                if ht in {"WALL","HALF_WALL","DOORWAY","WINDOW_FRAME","WALL_FRAME"} and is_ceil:
                    self.obj.location = Vector((
                        obj.location.x,
                        obj.location.y,
                        get_wall_top_z(obj) - THICK   # flush baby
                    ))
                    self.obj.rotation_euler = obj.rotation_euler.copy()
                    return {'RUNNING_MODAL'}

                elif is_foundation(obj):

                    if is_square_foundation(obj):
                        if is_tri and pt == "TRI_FOUNDATION":
                            place_on_edge(hit, obj, self.obj)
                            return {'RUNNING_MODAL'}
                        if is_ceil and pt == "CEILING":
                            self.obj.location = snap_square(hit, obj)
                            return {'RUNNING_MODAL'}
                        if is_wall_like:
                            place_on_edge(hit, obj, self.obj)
                            self.obj.rotation_euler.z += radians(180)   # flip to the good side damnit
                            self.obj.location.z = get_foundation_top_z(obj)
                            return {'RUNNING_MODAL'}
                        self.obj.location = snap_square(hit, obj)

                    elif is_tri_foundation(obj):
                        if is_ceil and pt == "TRI_CEILING":
                            self.obj.location = snap_tri_grid(hit, obj)
                            return {'RUNNING_MODAL'}
                        if is_wall_like:
                            place_on_edge(hit, obj, self.obj)
                            self.obj.rotation_euler.z += radians(180)
                            self.obj.location.z = get_foundation_top_z(obj)
                            return {'RUNNING_MODAL'}
                        if pt == "TRI_FOUNDATION":
                            place_on_edge(hit, obj, self.obj)
                            return {'RUNNING_MODAL'}
                        self.obj.location = snap_tri_grid(hit, obj)

            else:
                if pt in {"SQUARE_FOUNDATION","TRI_FOUNDATION"}:
                    g = ground_hit(context, event)
                    if g:
                        self.obj.location = snap_world_grid(g)
                        self.obj.rotation_euler = (0,0,0)

        if event.type == 'LEFTMOUSE' and event.value == 'PRESS':
            self.obj.select_set(True)
            return {'FINISHED'}

        return {'RUNNING_MODAL'}


# ---------------- pie ----------------

class REGENESIS_MT_build_pie(bpy.types.Menu):
    bl_label = "Regenesis Shit"
    bl_idname = "REGENESIS_MT_build_pie"

    def draw(self, context):
        pie = self.layout.menu_pie()
        pie.operator("regenesis.place_piece", text="Square").piece_type = "SQUARE_FOUNDATION"
        pie.operator("regenesis.place_piece", text="Wall").piece_type = "WALL"
        pie.operator("regenesis.place_piece", text="Ceiling").piece_type = "CEILING"
        pie.operator("regenesis.place_piece", text="Door").piece_type = "DOORWAY"
        pie.operator("regenesis.place_piece", text="Tri").piece_type = "TRI_FOUNDATION"
        pie.operator("regenesis.place_piece", text="Window").piece_type = "WINDOW_FRAME"
        pie.operator("regenesis.place_piece", text="TriCeil").piece_type = "TRI_CEILING"
        pie.operator("regenesis.place_piece", text="Half").piece_type = "HALF_WALL"


class REGENESIS_OT_call_build_pie(bpy.types.Operator):
    bl_idname = "regenesis.call_build_pie"
    bl_label = "Call Build Pie"

    def execute(self, context):
        bpy.ops.wm.call_menu_pie(name="REGENESIS_MT_build_pie")
        return {'FINISHED'}


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
        km = kc.keymaps.new(name='3D View', space_type='VIEW_3D')
        kmi = km.keymap_items.new('regenesis.call_build_pie', 'Q', 'PRESS')
        addon_keymaps.append((km, kmi))


def unregister():
    for km, kmi in addon_keymaps:
        km.keymap_items.remove(kmi)
    addon_keymaps.clear()
    for c in reversed(classes):
        bpy.utils.unregister_class(c)


if __name__ == "__main__":
    register()
